open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
//Bootstrap paket
let dst = ".paket\paket.exe"
if not (File.Exists dst) then
    let urlRef = @"https://fsprojects.github.io/Paket/stable"
    use wc = new Net.WebClient()
    let url = wc.DownloadString(urlRef)
    let tmp = Path.GetTempFileName()
    wc.DownloadFile(url, tmp)
    Directory.CreateDirectory(".paket") |> ignore
    File.Move(tmp, dst)
 
//Resolve and install the packages
 
#r ".paket\paket.exe"
 
Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget FSharp.Data
"""
//-------------------------------------------------------------------------------
//Use the packages
#r @"packages\FSharp.Data\lib\net40\FSharp.Data.dll"
#r "System.Drawing.dll"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open System.Net
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Drawing

type TweetJSONType = JsonProvider<"tweet.json">
type PlaceJSONType = JsonProvider<"place.json">
let StreamBaseUrl = "https://stream.twitter.com"
let consumer_key =  "zhpLaIqlrIZ7au6Ihgeg" //Consumer Key (API Key) //test: "xvz1evFS4wEEPTGEFPHBog"
let consumer_secret =  "qWgIiNVmLgQo4iN4N0v6mQ3sZgYq2oI6DUdI3ItoyqU" //Consumer Secret (API Secret) //test: "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
let access_token = "97693175-MsNwT42Wm7C4Q19WSPQzm4R7hOaijds4dwjszyKYp" //Acces Token //test: "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
let access_token_secret =  "EpOYF35WK8yj3GsAOLaKZCJTqxZyN5GCO4aGsinYL5uqc" //test: "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"


//-------- jpg to ascii begin
let Color2Gray (c:Color) =
    //((float)c.R * 0.3) + ((float)c.G * 0.59) + ((float)c.B * 0.11)
    ((float)c.R * 0.2126) + ((float)c.G * 0.7152) + ((float)c.B * 0.0722)

let Image2Ascii (img:Image) = 
    let mutable n_h = 60
    let mutable n_w = 80

    n_h <- (int)((float)img.Height * (float)n_w/(2.1 *(float)img.Width)) //calculate resized height

    if n_h > 60 then
        n_h <- 60
        n_w <- (int)((float)img.Width * 2.1 * (float)n_h/(float)img.Height)

    use bmp = new Bitmap(img,new Size(n_w,n_h)) //resize
    //let asciiMap = [|' ';'.';',';':';';';'o';'x';'%';'#';'@'|]
    let asciiMap = [|'`';'\'';'.';',';':';';';'i';'+';'o';'*';'%';'&';'$';'#';'@'|]
    for y in [|0 .. bmp.Size.Height-1|] do
        for x in [|0 .. bmp.Size.Width-1|] do
           //convert to grayscale
           let grayScale = Color2Gray (bmp.GetPixel(x,y))
           let scaled = (grayScale * float(Array.length(asciiMap)-1) /255.0)
           let index = (int)(scaled + 0.5)
           printf "%c" asciiMap.[index]
        printfn ""
    printfn ""
//---------- jpg to ascii end

let ResolveShortURL (url:string) = 
    let request = WebRequest.Create(url);
    request.Method <- WebRequestMethods.Http.Head;
    let response = request.GetResponse();
    response.ResponseUri.ToString();

let ExtractUrl (text:string) =
    // assume that a url exists in text; if not exception thrown
    let words = text.Split([|" ";"\n"|], StringSplitOptions.RemoveEmptyEntries)
    words |> Array.find(fun k-> k.Trim().StartsWith("http://") || k.StartsWith("https://"))

let ExtractImage (url:string) =
    use wc1 = new WebClient()
    let h = wc1.DownloadData(new Uri(url));
    use hstream = new MemoryStream(h);
    let htmlDoc = HtmlDocument.Load(hstream)

    let imgs =
        htmlDoc.Descendants ["img"]
        |> Seq.choose (fun x -> 
                x.TryGetAttribute("src")
                |> Option.map (fun a -> a.Value())
        )
        |> Seq.filter(fun j-> j.EndsWith("jpg") && (not <| j.Contains("profile_images")))

    if Seq.length(imgs) > 0 then
        imgs 
        |> Seq.head
        |> fun im->
            use wc = new WebClient()
            let imageData = wc.DownloadData(im);
            use imgStream = new MemoryStream(imageData);
            let img = Image.FromStream(imgStream);
            Image2Ascii img

let GetUnixEpochTimeStamp = 
    //test: "1318622958"
    let secs = (System.DateTime.UtcNow.Subtract(new DateTime(1970,1,1))).TotalSeconds
    Convert.ToInt64(secs).ToString(System.Globalization.CultureInfo.InvariantCulture)

let GetNonce = 
    //test: let nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" 
    Guid.NewGuid().ToString().Replace("-", String.Empty) //32 alphanumeric chars

let EncodeChar (c:char) = 
    let bytes = System.Text.Encoding.UTF8.GetBytes([|c|])
    bytes 
        |> Seq.map(fun b -> "%" + b.ToString("X2")) 
        |> System.String.Concat

let PercentEncode (s:string) = 
    let chars = s.ToCharArray()
    chars 
        |> Seq.map(fun c->
            match c with
            | c when c >= '0' && c <= '9'  -> System.Convert.ToString(c)
            | c when c >= 'a' && c <= 'z'  -> System.Convert.ToString(c)
            | c when c >= 'A' && c <= 'Z'  -> System.Convert.ToString(c)
            | '.' | '-' | '_' |'~' -> System.Convert.ToString(c)
            | _ -> EncodeChar(c)
            )
        |> System.String.Concat

let Sign_HMAC_SHA1 (sig_ready:string) =
    let signing_key = (PercentEncode consumer_secret) + "&" + (PercentEncode access_token_secret)
    let keyBytes = System.Text.Encoding.ASCII.GetBytes(signing_key)
    use hmac = new System.Security.Cryptography.HMACSHA1(keyBytes)
    let byteArray = System.Text.Encoding.ASCII.GetBytes(sig_ready)
    use mem_stream = new MemoryStream(byteArray)
    let signed = hmac.ComputeHash(mem_stream)
    Convert.ToBase64String(signed,Base64FormattingOptions.None)

let ComposeSignatureParamString (keyvalues:SortedDictionary<string,string>) = 
    keyvalues
        |> Seq.sortBy(fun (KeyValue(k,v)) -> PercentEncode k ) //twitter needs keys to be percent encoded before sorting
        |> Seq.map(fun kvp -> (PercentEncode kvp.Key) + "=" + (PercentEncode kvp.Value) + "&" )
        |> System.String.Concat
        |> fun k-> k.TrimEnd('&') //remove the excess & at the end

let ComposeOAuthString (keyvalues:SortedDictionary<string,string>) (sig_base_string:string) = 
    keyvalues.Add("oauth_consumer_key",consumer_key)
    keyvalues.Add("oauth_token",access_token)
    keyvalues.Add("oauth_nonce", GetNonce)
    keyvalues.Add("oauth_version","1.0")
    keyvalues.Add("oauth_timestamp", GetUnixEpochTimeStamp)
    keyvalues.Add("oauth_signature_method","HMAC-SHA1")

    let param_string = ComposeSignatureParamString keyvalues
    let pre_signed_sig = sig_base_string + "&" + (PercentEncode param_string) 
    let signed_sig = Sign_HMAC_SHA1 pre_signed_sig
    keyvalues.Add("oauth_signature", signed_sig)

    //Authorization header should only contain oauth_ prefixed keys
    keyvalues 
        |> Seq.filter(fun x-> x.Key.StartsWith("oauth_"))
        |> Seq.map(fun x-> PercentEncode x.Key + "=" + "\"" + PercentEncode x.Value + "\", ")
        |> System.String.Concat
        |> fun k->k.TrimEnd(' ',',')

let ReadStreamingResponse (w:WebRequest) = 
    try
        let resp = w.GetResponse() :?> HttpWebResponse
        use responseStream = resp.GetResponseStream()
        
        use responseReader = new StreamReader(responseStream, true)
        let atEnd = responseReader.EndOfStream
        while(not atEnd) do
            let line_size = responseReader.ReadLine()
            if not (String.IsNullOrEmpty line_size) then
                let size = int line_size
                let buffer = Array.zeroCreate size
                let numread = responseReader.ReadBlock(buffer,0,size)
                let tweet = new System.String(buffer) |> System.Net.WebUtility.HtmlDecode |> TweetJSONType.Parse
                let place = tweet.Place.ToString() |> PlaceJSONType.Parse
                
                if not (tweet.Text.StartsWith("@")) then
                    printfn "%s(%s) - At %s-%s\n%s" 
                            tweet.User.Name tweet.User.ScreenName 
                            place.Name place.FullName tweet.Text
                try
                if tweet.Text.Contains("http://") || tweet.Text.Contains("https://") then
                    let shortUrl = ExtractUrl(tweet.Text)
                    ExtractImage shortUrl
                else
                    printfn "..................................................................\n"
                with
                //| :? System.Net.WebException as ex1-> printfn "*****Error: %s\n\n" ex1.Message
                | :? System.Net.WebException -> printfn ""
    with
        | :? WebException as ex -> printfn "Error %O" ex;

let GetStreamingWebResource (endpoint:string) (optional_params:SortedDictionary<string,string>) =
    let url = StreamBaseUrl + endpoint
    let sig_base_string = WebRequestMethods.Http.Get + "&" + (PercentEncode url)
    let dict = optional_params

    let query_string = 
        dict 
        |> Seq.map( fun kv-> String.Format("{0}={1}", (System.Net.WebUtility.UrlEncode kv.Key), 
                                                    (System.Net.WebUtility.UrlEncode kv.Value)) )
        |> Seq.fold(fun s1 s2-> s1 + s2 + "&") "?"

    let url = endpoint + query_string.TrimEnd(' ','&')
    let w = WebRequest.Create(StreamBaseUrl + url) :?> HttpWebRequest
    w.UserAgent <- "ixmx"

    let oauth = ComposeOAuthString dict sig_base_string
    w.Headers.Add("Authorization", "OAuth " + oauth)
    w.Method <- WebRequestMethods.Http.Get
    w.ContentType <- "application/x-www-form-urlencoded"
    w

let StreamingListenerToLocation  location =
    printfn "\n Streaming Listner Started..\n Listening to Tweets from location %s \n" location
    let endpoint = "/1.1/statuses/filter.json"
    let dict = new System.Collections.Generic.SortedDictionary<string,string>()
    dict.Add("delimited","length");
    dict.Add("locations", location)
    let w = GetStreamingWebResource endpoint dict
    ReadStreamingResponse w

//System.Console.OutputEncoding <- System.Text.Encoding.Unicode
let newyork = "-74,40,-73,41"
let tokyo = "139,35,140,36"
let cambridge = "0.004,52.16,0.28,52.29"
StreamingListenerToLocation tokyo


    
