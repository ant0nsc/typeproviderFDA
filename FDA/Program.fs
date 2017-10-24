module Program

open ApiParser
open System
open System.IO

/// Writes all strings to the given file.
let writeAllLines file (lines: string seq) = File.WriteAllLines(file, lines)

/// Prints tuples tab-separated to the given file.
let tupleToFile file lines =
    lines
    |> Seq.map (fun (t1, t2) -> sprintf "%A\t%A" t1 t2)
    |> writeAllLines file

/// Prints 3-tuples tab-separated to the given file.
let tuple3ToFile file lines =
    lines
    |> Seq.map (fun (t1, t2, t3) -> sprintf "%A\t%A\t%A" t1 t2 t3)
    |> writeAllLines file

/// Prints 4-tuples tab-separated to the given file.
let tuple4ToFile file lines =
    lines
    |> Seq.map (fun (t1, t2, t3, t4) -> sprintf "%A\t%A\t%A\t%A" t1 t2 t3 t4)
    |> writeAllLines file

/// Flattens tuples that contains a count lookup into individual entries
/// (denormalized table)
let unroll lines =
    lines
    |> Seq.collect (fun (country, numEntries, reactionCount) ->
        reactionCount
        |> Seq.map (fun (reaction, count) -> country, numEntries, reaction, count)
    )

/// Some random bits where I played around with the API
let workingWithRawRecords startDate endDate numEntries =
    let records = ApiParser.GetRecords startDate endDate numEntries
    printfn "Got %i records" records.Length

    let missing = "(missing)"

    let GetCountry (r: FdaFullRecord.Result) =
        match r.Primarysource with
        | Some country -> 
            match country.Reportercountry with
            | Some c -> c
            | _ -> missing
        | _ -> missing

    let GetReactions (r: FdaFullRecord.Result) =
        r.Patient.Reaction
        |> Seq.map (fun r -> r.Reactionmeddrapt)

    let country = 
        records
        |> Seq.map GetCountry
    
    let numReportsByCountry = 
        country 
        |> Seq.countBy id
        |> Seq.toList

    numReportsByCountry
    |> tupleToFile "C:/temp/reportByCountry.tsv"

    let numReactionsByCountry =
        records
        |> Seq.map (fun r ->
            let country = GetCountry r
            let numReactions = GetReactions r |> Seq.length
            country, float numReactions
        )

    let averageReactionsPerCountry =
        numReactionsByCountry
        |> Seq.groupBy fst
        |> Seq.map (fun (country, reports) -> 
            country, Seq.length reports, Seq.averageBy snd reports
        )

    averageReactionsPerCountry
    |> tuple3ToFile "C:/temp/averageReactionsPerCountry.tsv"

[<EntryPoint>]
let main argv = 

    let numCountries = 10

    // This reports several countries twice: US and UNITED STATES,
    // DE and GERMANY. Ignore for now, because I would have to create a 
    // list of country name synonyms.

    let byCountry = 
        GetCounts "" countryField true numCountries
        |> Seq.toList
    byCountry
    |> tupleToFile "C:/temp/byCountry.tsv"

    /// The TOP 5 reported reactions per country, as a list of
    /// (country, numReports, (reaction, N))
    let reactionsByCountry = 
        byCountry
        |> Seq.truncate numCountries
        |> Seq.map (fun (c, numEntries) ->
            let counts = GetCounts (getCountryCondition c) reactionField true 5
            c, numEntries, counts
        )

    reactionsByCountry
    |> unroll
    |> tuple4ToFile "c:/temp/topReactionsByCountry.tsv"

    /// The per-country breakdown of patient sex, as a list of
    /// (country, numReports, (sex, N))
    let sexByCountry =
        byCountry
        |> Seq.truncate numCountries
        |> Seq.map (fun (c, numReports) ->
            let counts = GetCounts (getCountryCondition c) sexField false 3
            c, numReports, counts
        )
        |> Seq.toList
    sexByCountry
    |> unroll
    |> tuple4ToFile "c:/temp/sexByCountry.tsv"

    let sexMale = "1"
    let sexFemale = "2"
    let sexNone = "0"
    /// Per country, list the number of reports, the fraction of reports
    /// with sex reported as female, and the fraction of reports where 
    /// no sex is present.
    let sexRatio = 
        sexByCountry
        |> Seq.map (fun (country, numReports, counts) ->
            let sexCounts = counts |> Map.ofSeq
            let fracFemale = (float sexCounts.[sexFemale]) / (float numReports)
            let fracNone = (float sexCounts.[sexNone]) / (float numReports)
            country, numReports, fracFemale, fracNone
        )
    sexRatio
    |> tuple4ToFile "c:/temp/sexRatio.tsv"


    // Take the country with max number of reports where the sex ratio is roughly 
    // balanced (GB) and the country with the largest imbalance (US).
    // JP has more males, BR is similar to the US
    // All GB, JP, BR report vastly fewer cases than the US (factor 10 or more)
    let countries = ["US"; "GB"; "JP"; "BR"]
    /// Creates a per-country per-sex breakdown, counting the given field
    /// and returning the top N
    let countByCountryAndSex countField limit =
        countries
        |> Seq.collect (fun country ->
            [ sexFemale; sexMale ]
            |> Seq.map (fun sex ->
                let filter = 
                    getCountryCondition country
                    |> joinFilterCondition (getFilterCondition sexField sex)
                let counts = GetCounts filter countField true limit
                country, sex, counts
            )
        )

    /// A breakdown of most frequent symptoms per sex and country
    let reactionsByCountryAndSex = countByCountryAndSex reactionField 3
    reactionsByCountryAndSex
    |> unroll
    |> tuple4ToFile "c:/temp/reactionsByCountryAndSex.tsv"

    // The different reactions could be due to different drugs that are 
    // being described:
    // Which drug classes are most frequently involved? Again break down by
    // country and sex
    let drugclassByCountryAndSex = countByCountryAndSex pharmclassField 3
    drugclassByCountryAndSex
    |> unroll
    |> tuple4ToFile "c:/temp/drugclassByCountryAndSex.tsv"

    // Actually we would like to see a breakdown by country, sex,
    // reported reaction AND pharmacological class, but that's really
    // awkward with the limited API.

    // Now get some time series, to see why there is such a spike in reports
    // around 2010

    /// Number of reports per day
    let reportsPerDay = 
        let filter = receivedateField + ":[20040101+TO+20171021]"
        GetTimeseries filter receivedateField false 100
        |> Seq.toList

    /// Aggregates a per-day sequence into a per-year sequence
    let getReportsPerYear reportsPerDay = 
        reportsPerDay
        |> Seq.groupBy (fun (date,_) -> getYearFromInt date)
        |> Seq.map (fun (year, entries) -> 
            year, entries |> Seq.sumBy snd
        )
    reportsPerDay
    |> getReportsPerYear
    |> tupleToFile "c:/temp/reportsPerYear.tsv"

    /// Per-day number of reports for US only
    let reportsPerDayUS = 
        let filter = 
            let cond1 = receivedateField + ":[20040101+TO+20171021]"
            let cond2 = getFilterCondition countryField "US"
            let cond3 = getFilterCondition countryField "UNITED STATES"
            joinFilterCondition (orFilterCondition cond2 cond3) cond1
        GetTimeseries filter receivedateField false 100
        |> Seq.toList
    reportsPerDayUS
    |> getReportsPerYear 
    |> tupleToFile "c:/temp/reportsPerYearUS.tsv"

    0