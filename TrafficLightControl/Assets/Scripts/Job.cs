using UnityEngine;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System;
using System.Text;

public class Job : ThreadedJob {

    private Process prolog;
    private StreamWriter sw;

    //overrides
    protected override void ThreadFunction() {
        initPrologProcess();
    }

    protected override void OnFinished() {

    }

    //"normal" functions

    public void initPrologProcess() {
        prolog = new Process();
        prolog.StartInfo.UseShellExecute = false;
        prolog.StartInfo.RedirectStandardOutput = true;
        prolog.StartInfo.RedirectStandardInput = true;

        prolog.StartInfo.FileName = @"C:\Program Files\swipl\bin\swipl.exe";

        prolog.Start();

        //IO
        sw = new StreamWriter(prolog.StandardInput.BaseStream, Encoding.ASCII);
        sw.AutoFlush = true;

        //Encoding
        Console.OutputEncoding = Encoding.ASCII;
        Console.InputEncoding = Encoding.ASCII;

        prolog.OutputDataReceived += (sender, args) => {
            printToUnity(args.Data);
        };

        prolog.BeginOutputReadLine();
        prolog.WaitForExit();
    }

    public void consultFile(string path) {
        if (prolog == null)
            return;

        //print("Try to consult: " + path);
        Console.WriteLine("Try to consult: " + path);
        printToUnity("Try to consult: " + path);


        sw.WriteLine("consult('" + path + "').");
        sw.Flush();

        //string output = prolog.StandardOutput.ReadToEnd();
        //printToUnity("consult '" + Path.GetFileName(path) + "': " + output);


    }

    public void printToUnity(string message) {
        //print("<-- " + message);
        Console.WriteLine("<-- " + message);

        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\out.txt")) {
            sw2.WriteLine(message);
            sw2.Flush();
        }
        
    }


    public void toProlog(string message) {

        if (sw == null)
            return;

        sw.WriteLine(message);        
    }   
}
