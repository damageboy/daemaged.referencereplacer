open Mono.Cecil;
open Mono.Cecil.Pdb;

open System.IO;
open System.Text.RegularExpressions;

let patchAssembly (targetAsm : string) (targetPatchedAsm : string) (re : Regex) (replace : string) nopdb =
  let targetInfo = new FileInfo(targetAsm)
  let pdbFileName = targetInfo.FullName.Remove(targetInfo.FullName.Length - targetInfo.Extension.Length) + ".pdb"
  let pdbExists = (not nopdb) && File.Exists(pdbFileName) 
  let ar = new DefaultAssemblyResolver()
  ar.AddSearchDirectory(@"c:\Windows\Microsoft.NET\Framework64\v4.0.30319")
  let rp = new ReaderParameters(AssemblyResolver = ar, 
                                ReadSymbols = pdbExists, 
                                ReadingMode = ReadingMode.Immediate)

  if (rp.ReadSymbols) then rp.SymbolReaderProvider <- new PdbReaderProvider()

  let ad = AssemblyDefinition.ReadAssembly(targetAsm, rp)

  let md = ad.Modules.[0]
  md.AssemblyReferences |> Seq.filter (fun r -> re.IsMatch(r.Name)) |> Seq.iter (fun r -> r.Name <- replace)

  //let newPdb = new FileStream(targetPatchedAsmPdb, FileMode.Create)
  let wp = new WriterParameters(WriteSymbols = pdbExists)
  if (wp.WriteSymbols) then wp.SymbolWriterProvider <- new PdbWriterProvider()
  ad.Write(targetPatchedAsm, wp)
  ignore

[<EntryPoint>]  
let main (args : string[]) =
  let targetAsm = args.[0]
  let targetPatchedAsm = args.[1]
  let re = new Regex(args.[2])
  let replace = args.[3]
  let nopdb = args.[4] = "nopdb"
  patchAssembly targetAsm targetPatchedAsm re replace nopdb
  0