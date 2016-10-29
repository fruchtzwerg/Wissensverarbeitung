using UnityEngine;
using System.Collections;
using System;

public class PrologTestScriptWithInterface : MonoBehaviour, IProlog {

    public GameObject PrologInterface;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
	}

    public void ReciveDataFromProlog(string recivedData) {
        print("######################################" + recivedData);
    }


    public void testProlog() {
        PrologInterface.GetComponent<PrologWrapper>().QueryProlog("version.", this);
    }
}
