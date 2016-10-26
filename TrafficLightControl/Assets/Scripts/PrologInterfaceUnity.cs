using UnityEngine;
using System.Collections;
using SbsSW.SwiPlCs;
using System;
using JJC.Psharp.Resources;
using JJC.Psharp.Predicates;
using JJC.Psharp.Lang;
using JJC.Psharp.Lang.Resource;
using System.Reflection;
using System.IO;
using System.Text;

public class PrologInterfaceUnity : MonoBehaviour {

    // Use this for initialization
    void Start() {
        Prolog();
    }

    // Update is called once per frame
    void Update() {

    }


    void Prolog() {
        #region old
        /*
        //Environment.SetEnvironmentVariable("SWI_HOME_DIR", @"the_PATH_to_boot32.prc");  // or boot64.prc
        if (!PlEngine.IsInitialized) {
            
            String[] param = { "-q" };  // suppressing informational and banner messages
            print("1");
            //PlEngine.Initialize(param);
            print("2");

            PlQuery.PlCall("assert(father(martin, inka))");
            PlQuery.PlCall("assert(father(uwe, gloria))");
            PlQuery.PlCall("assert(father(uwe, melanie))");
            PlQuery.PlCall("assert(father(uwe, ayala))");
            print("3");
            using (var q = new PlQuery("father(P, C), atomic_list_concat([P,' is_father_of ',C], L)")) {
                foreach (PlQueryVariables v in q.SolutionVariables)
                    print(v["L"].ToString());

                print("all children from uwe:");
                q.Variables["P"].Unify("uwe");
                foreach (PlQueryVariables v in q.SolutionVariables)
                    print(v["C"].ToString());
            }
            PlEngine.PlCleanup();
            print("finshed!");
        }*/
        #endregion


               
        TextReader tr = new StreamReader(@".\Assets\Prolog\test.pl");
        PushbackReader pbr = new PushbackReader(tr);

        TextWriter tw = new StreamWriter(@".\Assets\Prolog\out.txt");
        TextWriter errorTw = new StreamWriter(@".\Assets\Prolog\error.log");

        PrologInterface prologInterface = new PrologInterface(pbr, tw, errorTw);

        //prologInterface.AddCallingAssembly();
        prologInterface.AddAssembly(System.Reflection.Assembly.GetExecutingAssembly());

        print(prologInterface.Call());

        prologInterface.Stop();

        print("finish..." + prologInterface.engine.current_input.Read());
    }
}