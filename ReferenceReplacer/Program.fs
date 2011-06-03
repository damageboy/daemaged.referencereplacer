open Mono.Cecil;
open Mono.Cecil.Pdb;

open System;
open System.IO;
open System.Collections.Generic;
open System.Threading.Tasks
open System.Text.RegularExpressions;
open Microsoft.FSharp.Text
open System.Reflection;


let patchAssembly targetAsm targetPatchedAsm (re : seq<list<Regex>>) (replace : seq<string>) nopdb verbose snkp =
  // Read the existing assembly
  let targetInfo = new FileInfo(targetAsm)
  let pdbFileName = targetInfo.FullName.Remove(targetInfo.FullName.Length - targetInfo.Extension.Length) + ".pdb"
  let pdbExists = (not nopdb) && File.Exists(pdbFileName) 
  let ar = new DefaultAssemblyResolver()
  ar.AddSearchDirectory(@"c:\Windows\Microsoft.NET\Framework64\v4.0.30319")
  let rp = new ReaderParameters(AssemblyResolver = ar, 
                                ReadSymbols = pdbExists,
                                ReadingMode = ReadingMode.Immediate)

  // Read the symbols if necessary/specified
  if (rp.ReadSymbols) then rp.SymbolReaderProvider <- new PdbReaderProvider()

  if verbose then
    let out = match rp.ReadSymbols with
      | true -> ""
      | false -> "out"
    printfn "Reading %A with%A symbols" targetAsm out

  // Do it
  let ad = AssemblyDefinition.ReadAssembly(targetAsm, rp)

  let nullsnkp = ref (null : StrongNameKeyPair)
  if snkp <> nullsnkp then
    let adName = ad.Name
    adName.HashAlgorithm <- AssemblyHashAlgorithm.SHA1
    adName.PublicKey <- (!snkp).PublicKey    
    ad.Name.Attributes <- ad.Name.Attributes ||| AssemblyAttributes.PublicKey

  // Replace all asm refs that match the list of regex to the new target name
  let md = ad.Modules.[0]
  
  let printdbg s1 s2 = 
    if verbose then
      printfn "Changing %A <- %A" s1 s2
    true

  let  hasChanges = ref false

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

  if !hasChanges then
    // Save the new asm
    let wp = new WriterParameters(WriteSymbols = pdbExists, StrongNameKeyPair = !snkp)
    if (wp.WriteSymbols) then wp.SymbolWriterProvider <- new PdbWriterProvider()

    if verbose then
      let out = match wp.WriteSymbols with
        | true -> ""
        | false -> "out"
      printfn "Reading %A with%A symbols" targetAsm out

    ad.Write(new FileStream(targetPatchedAsm, FileMode.Create), wp)

[<EntryPoint>]  
let main (args : string[]) =

  let argList = new List<string>()
  let addArg s = argList.Add(s)
  
  let replace = new List<string>()
  let verbose = ref false
  let matchList = new List<list<Regex>>()
  let noPdb = ref false
  let skipMissing = ref false
  let useTasks = ref false
  let keyPair = ref (null : StrongNameKeyPair)
  
  let snkp fn = new StrongNameKeyPair(File.Open(fn, FileMode.Open))

  
  let specs =
    ["-v",             ArgType.Set verbose,                                    "Display additional information"
     "--nopdb",        ArgType.Set noPdb,                                      "Skip creation of PDB files"
     "--skip-missing", ArgType.Set skipMissing,                                "Skip missing input files silently"   
     "--parallel",     ArgType.Set useTasks,                                   "Execute task in parallel on all avaiable CPUs"   
     "--match",        ArgType.String (fun s -> s.Split(',') |> 
                                           Seq.map (fun p -> new Regex(p)) |> 
                                           List.ofSeq |> matchList.Add),       "Reference match list separated by commas"
     "--replace",      ArgType.String (fun s -> replace.Add(s)),               "Replace matches with"
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
     | 1 -> [output]
     | _ -> realInputList |> List.map (fun fi -> Path.Combine(output, fi.Name))
  
  if (!verbose) then
    printfn "outputList=%A" outputList
    

  if !useTasks then
    let tasks = 
      outputList 
      |> Seq.zip realInputList 
      |> Seq.map (fun (i, o) -> Task.Factory.StartNew(new Action(fun () -> patchAssembly i.FullName o matchList replace !noPdb !verbose keyPair)))    
      |> Seq.toArray
    Task.WaitAll(tasks)    
  else
    outputList 
    |> Seq.zip realInputList  
    |> Seq.iter (fun (i, o) -> patchAssembly i.FullName o matchList replace !noPdb !verbose keyPair)

  0