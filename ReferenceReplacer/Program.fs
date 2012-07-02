open Mono.Cecil;
open Mono.Cecil.Pdb;

open System;
open System.IO;
open System.Collections.Generic;
open System.Threading.Tasks
open System.Text.RegularExpressions;
open Microsoft.FSharp.Text
open System.Reflection;
open Mono.Unix.Native;

let notWindows = Environment.OSVersion.Platform <> PlatformID.Win32NT
let isWindows = not notWindows

let readAsm sourceAsm noPdb searchDirs verbose =
  // Read the existing assembly
  let sourceInfo = new FileInfo(sourceAsm)
  let pdbFileName = sourceInfo.FullName.Remove(sourceInfo.FullName.Length - sourceInfo.Extension.Length) + ".pdb"
  let pdbExists = (not noPdb) && File.Exists(pdbFileName) 
  let ar = new DefaultAssemblyResolver()

  // Add search directories such as @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0"
  searchDirs |> Seq.iter (fun sd -> ar.AddSearchDirectory(sd))

  
  let rp = new ReaderParameters(AssemblyResolver = ar, 
                                ReadSymbols = pdbExists,
                                ReadingMode = ReadingMode.Immediate)
  
  // Read the symbols if necessary/specified
  if (rp.ReadSymbols) then rp.SymbolReaderProvider <- new PdbReaderProvider()
  if verbose then
    let out = 
      match rp.ReadSymbols with
      | true -> ""
      | false -> "out"
    printfn "Reading %A with%s symbols" sourceAsm out
  
  // Do it
  try
    AssemblyDefinition.ReadAssembly(sourceAsm, rp)
  with
  | _ as ex
    ->  raise <| new Exception(String.Format("Failed to assembly {0}", sourceAsm), ex)

let replaceReferences (re : seq<list<Regex>>) (replace : seq<string>) verbose (ad : AssemblyDefinition) =
  // Replace all asm refs that match the list of regex to the new target name
  let md = ad.MainModule
  
  let printdbg s1 s2 = 
    if verbose then
      printfn "Changing %A <- %A" s1 s2
    true

  let hasChanges = ref false

  replace |> 
  Seq.zip re |> 
  Seq.iter (fun (reList, replaceStr) -> 
    md.AssemblyReferences |> 
    Seq.filter (fun r -> reList |> Seq.exists (fun x -> x.IsMatch(r.Name))) |> 
    Seq.filter (fun r -> printdbg r.Name replaceStr) |>
    Seq.iter (fun r -> 
      do
        r.Name <- replaceStr
        hasChanges := true
    ))
  !hasChanges

let writeAsm destAsm noPdb verbose snkp (ad : AssemblyDefinition) = 
  // Force signing the new assembly if we were passed a SNK file ref
  let nullsnkp = ref (null : StrongNameKeyPair)
  if snkp <> nullsnkp then
    let adName = ad.Name
    adName.HashAlgorithm <- AssemblyHashAlgorithm.SHA1
    adName.PublicKey <- (!snkp).PublicKey    
    ad.Name.Attributes <- ad.Name.Attributes ||| AssemblyAttributes.PublicKey
 
  let wp = new WriterParameters(WriteSymbols = ad.MainModule.HasSymbols, StrongNameKeyPair = !snkp)
  if (wp.WriteSymbols) then wp.SymbolWriterProvider <- new PdbWriterProvider()

  if verbose then
    let out = 
      match wp.WriteSymbols with
      | true -> ""
      | false -> "out"
    printfn "Writing %A with%A symbols" destAsm out

  ad.Write(new FileStream(destAsm, FileMode.Create), wp)
  
  if notWindows && Path.GetExtension(destAsm) = ".exe" then    
    printfn "Chmodding %A to executable" destAsm
    Syscall.chmod(destAsm, FilePermissions.S_IRWXU ||| FilePermissions.S_IRGRP ||| FilePermissions.S_IXGRP ||| FilePermissions.S_IROTH ||| FilePermissions.S_IXOTH) |> ignore

    
    

let patchAssembly sourceAsm destAsm (re : seq<list<Regex>>) (replace : seq<string>) noPdb verbose snkp searchDirs =
  try 
    let ad = readAsm sourceAsm noPdb searchDirs verbose 
    let hasChanges = replaceReferences re replace verbose ad 
    if hasChanges then
      writeAsm destAsm noPdb verbose snkp ad
  with
  | _ as ex
    -> printfn "Encountered %A while trying to process %A" ex sourceAsm




[<EntryPoint>]  
let main (args : string[]) =

  let argList = new List<string>()
  let addArg s = argList.Add(s)
  
  let replace = new List<string>()
  let verbose = ref false
  let matchList = new List<list<Regex>>()
  let noPdb = ref false
  let skipMissing = ref false
  let checkTimeStamps = ref false
  let useTasks = ref false
  let keyPair = ref (null : StrongNameKeyPair)
  let searchDirs = new List<string>()
  
  let snkp fn = new StrongNameKeyPair(File.Open(fn, FileMode.Open))

  
  let specs =
    ["-v",                 ArgType.Set verbose,                                    "Display additional information"
     "--nopdb",            ArgType.Set noPdb,                                      "Skip creation of PDB files"
     "--skip-missing",     ArgType.Set skipMissing,                                "Skip missing input files silently"   
     "--check-timestamps", ArgType.Set checkTimeStamps,                            "Skip processing files where the target is newer than the source"   
     "--parallel",         ArgType.Set useTasks,                                   "Execute task in parallel on all available CPUs"   
     "--match",            ArgType.String (fun s -> s.Split(',') |> 
                                           Seq.map (fun p -> new Regex(p)) |> 
                                           List.ofSeq |> matchList.Add),       "Reference match list separated by commas"
     "--replace",      ArgType.String (fun s -> replace.Add(s)),               "Replace matches with"
     "--search-path",  ArgType.String 
                          (fun s -> searchDirs.AddRange(s.Split(';', ':'))),            "Base date for build date"     
     "--keyfile",      ArgType.String (fun s -> keyPair := snkp s),            "Key pair to sign the assembly with"
     "--",             ArgType.Rest   addArg,                                  "Stop parsing command line"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo(sh, ty, desc))
 
  let () =
    ArgParser.Parse(specs, addArg)

  let output = argList.[argList.Count - 1]
  argList.RemoveAt(argList.Count - 1)

  let inputList = List.ofSeq argList
  if (!verbose) then
    printfn "inputList=%A" inputList
    printfn "output=%A" output
    if !keyPair <> null then
      printfn "key-pair=%A" (!keyPair).PublicKey

  let realInputList = match !skipMissing with
    | true -> inputList |> List.map (fun i -> new FileInfo(i)) |> List.filter (fun fi -> fi.Exists)
    | false -> inputList |> List.map (fun i -> new FileInfo(i))

  if (not(!skipMissing) && (realInputList |> Seq.exists (fun x -> not(x.Exists)))) then
    printfn "Some input files are missing..., aborting"
    Environment.Exit(-1)
    
  let outputList = 
   match List.length inputList with
     | 1 -> [ new FileInfo(if Directory.Exists(output) then Path.Combine(output, Path.GetFileName(realInputList.[0].FullName)) else output)]
     | _ -> realInputList |> List.map (fun fi -> new FileInfo(Path.Combine(output, fi.Name)))
  
  if (!verbose) then
    printfn "outputList=%A" outputList
    
  let processPairs =
    outputList 
    |> Seq.zip realInputList 
    |> Seq.filter (fun (i, o) -> 
      match !checkTimeStamps with
      | true -> not(o.Exists) || i.LastWriteTimeUtc > o.LastWriteTimeUtc
      | false -> true
    )
    |> Seq.toList

  if !useTasks then
    let tasks = 
      processPairs
      |> Seq.map (fun (i, o) -> Task.Factory.StartNew(new Action(fun () -> patchAssembly i.FullName o.FullName matchList replace !noPdb !verbose keyPair searchDirs)))
      |> Seq.toArray
    Task.WaitAll(tasks)    
  else
    processPairs    
    |> Seq.iter (fun (i, o) -> patchAssembly i.FullName o.FullName matchList replace !noPdb !verbose keyPair searchDirs)

  0
