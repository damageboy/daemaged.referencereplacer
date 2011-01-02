open Mono.Cecil;
open Mono.Cecil.Pdb;

open System;
open System.IO;
open System.Collections.Generic;
open System.Text.RegularExpressions;
open Microsoft.FSharp.Text

let patchAssembly targetAsm targetPatchedAsm (re : seq<list<Regex>>) (replace : seq<string>) nopdb verbose =
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

  // Replace all asm refs that match the list of regex to the new target name
  let md = ad.Modules.[0]
  
  let printdbg s1 s2 = 
    if verbose then
      printfn "Changing %A <- %A" s1 s2
    true

  replace |> 
  Seq.zip re |> 
  Seq.iter (fun (reList, replaceStr) -> 
    md.AssemblyReferences |> 
    Seq.filter (fun r -> reList |> Seq.exists (fun x -> x.IsMatch(r.Name))) |> 
    Seq.filter (fun r -> printdbg r.Name replaceStr) |>
    Seq.iter (fun r -> r.Name <- replaceStr))
    
  // Save the new asm
  let wp = new WriterParameters(WriteSymbols = pdbExists)
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
  
  
  let specs =
    ["-v", ArgType.Set verbose, "Display additional information"
     "--nopdb", ArgType.Set noPdb, "Skip creation of PDB files"
     "--match", ArgType.String (fun s -> s.Split(',') |> Seq.map (fun p -> new Regex(p)) |> List.ofSeq |> matchList.Add), "Reference match list separated by commas"
     "--replace", ArgType.String (fun s -> replace.Add(s)), "Replace matches with"
     "--", ArgType.Rest addArg, "Stop parsing command line"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo(sh, ty, desc))
 
  let () =
    ArgParser.Parse(specs, addArg)

  let output = argList.[argList.Count - 1]
  argList.RemoveAt(argList.Count - 1)
  let inputList = List.ofSeq argList
  if (!verbose) then
    printfn "inputList=%A" inputList
    printfn "output=%A" output

  let outputList = 
   match List.length inputList with
     | 1 -> [output]
     | _ -> inputList |> List.map(fun i -> Path.Combine(output, i))
  
  if (!verbose) then
    printfn "outputList=%A" outputList
     
  //outputList  |> Seq.zip input |> Seq.iter (fun (i, o) -> printfn "patchAssembly %A %A %A %A %A" i o matchList !replace !noPdb)
  outputList  |> Seq.zip inputList |> Seq.iter (fun (i, o) -> patchAssembly i o matchList replace !noPdb !verbose)
  0