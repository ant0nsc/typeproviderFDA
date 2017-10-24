module ApiParser

open FSharp.Data

/// A Json parser that reads full FDA event records.
type FdaFullRecord = JsonProvider<"ExampleApiResponse.json">

/// A Json parser that reads the response to a count query,
/// with only two fields "term" and "count"
type FdaCountResponse = JsonProvider<"ExampleCountResponse.json">
/// Gets the results of querying the API with the given URI,
/// when we expect a count response.
let CountResponse (uri: string) = (FdaCountResponse.Load uri).Results

/// A Json parser that reads the response to a time series query,
/// with only two fields "time" and "count"
type FdaTimeSeries = JsonProvider<"ExampleTimeSeriesResponse.json">
/// Gets the results of querying the API with the given URI,
/// when we expect a time series response.
let TimeseriesResponse (uri: string) = (FdaTimeSeries.Load uri).Results

/// The access key to the API
let ApiKey = "Wmp4QrEdcegI9UN0y2sIDQszTiy16thCjYaVOWBt"
/// The first bits of the API URI, including the API key
let ApiPrefix = sprintf "https://api.fda.gov/drug/event.json?api_key=%s&" ApiKey

/// Gets the URI to retrieve a full list of FDA even records in a given time range,
/// limiting to the given number of entries. Note: In the best case,
/// the API will anyhow only report a maximum of 100 response items.
let GetFullRecordApi startDate endDate numEntries = 
    let limit = 
        match numEntries with
        | Some i -> sprintf "&limit=%i" i
        | _ -> ""
    sprintf 
        "%ssearch=receivedate:[%s+TO+%s]%s"
        ApiPrefix startDate endDate limit

/// Gets a list of FDA even records in a given time range,
/// limiting to the given number of entries. Note: In the best case,
/// the API will anyhow only report a maximum of 100 response items.
let GetRecords startDate endDate numEntries = 
    let fullApi = GetFullRecordApi startDate endDate numEntries
    (FdaFullRecord.Load fullApi).Results

/// Perform a search and count query on the FDA server.
/// Records will be restricted to the given condition, then a count of
/// distinct values of the field given in "countBy" is done.
/// Returns the entries with top count only, limiting to the given number
/// of response items.
let GetCountsOrTimeSeries api keyGetter valueGetter (condition: string) countBy isExact limit =
    let GetCountsApi condition countBy isExact limit =
        let exact = if isExact then ".exact" else ""
        sprintf "%ssearch=%s&count=%s%s&limit=%i" ApiPrefix condition countBy exact limit
    let condition = condition.Replace(' ', '+')
    let results =
        try
            let fullKey = GetCountsApi condition countBy isExact limit
            printfn "API: %s" fullKey
            api fullKey
        with
        | ex -> 
            printfn "Request failed: %s" ex.Message
            [| |]
    results
    |> Seq.map (fun r -> keyGetter r, valueGetter r)
    |> Seq.toList

/// Perform a search and count query on the FDA server.
/// Records will be restricted to the given condition, then a count of
/// distinct values of the field given in "countBy" is done.
/// Returns the entries with top count only, limiting to the given number
/// of response items.
let GetCounts = 
    GetCountsOrTimeSeries CountResponse (fun r -> r.Term) (fun r -> r.Count)

/// Perform a time series query on the FDA server.
/// Records will be restricted to the given condition, then a count of
/// distinct values of the field given in "countBy" is done.
/// Returns the entries with top count only, limiting to the given number
/// of response items.
let GetTimeseries = 
    GetCountsOrTimeSeries TimeseriesResponse (fun r -> r.Time) (fun r -> r.Count)

/// The field that contains the country in which an event was reported.
let countryField = "primarysource.reportercountry"
/// The field that contains the reaction that the patient reported.
let reactionField = "patient.reaction.reactionmeddrapt"
/// The field that contains the patient's sex.
let sexField = "patient.patientsex"
/// The field that contains the pharmacological class, EPC
let pharmclassField = "patient.drug.openfda.pharm_class_epc"
/// The field that contains the date when the report was received.
let receivedateField = "receivedate"

/// Creates a filter condition for the API, format: field:"value"
let getFilterCondition field value = sprintf "%s:\"%s\"" field value
/// Creates a filter condition string to restrict to the given country.
let getCountryCondition = getFilterCondition countryField
/// Joins 2 filter condition strings to form a logical AND
let joinFilterCondition cond1 cond2 = cond1 + "+AND+" + cond2
/// Joins 2 filter condition strings to form a logical OR
let orFilterCondition cond1 cond2 = sprintf "(%s)+(%s)" cond1 cond2

/// Gets the year from a date field that is an integer like 20170131.
let getYearFromInt dateAsInt = dateAsInt.ToString().Substring(0, 4)
