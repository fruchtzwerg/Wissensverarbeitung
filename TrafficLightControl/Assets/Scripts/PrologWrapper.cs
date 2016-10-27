using UnityEngine;
using System.Diagnostics;
using System.IO;
using System.Text;
using System;

public class PrologWrapper : MonoBehaviour {

    public string[] PrologFiles;

    Job prolog;

    // Use this for initialization
    void Start () {

        prolog = new Job();
        prolog.initPrologProcess();
        
        for (int i = 0; i < PrologFiles.Length; i++)
            prolog.consultFile(PrologFiles[i]);

        //prolog.toProlog("gewichtsumme([], 0).");
	}
	
	// Update is called once per frame
	void Update () {
       
	}
}
