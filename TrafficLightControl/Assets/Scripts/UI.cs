using UnityEngine;
using System.Collections;
using UnityEngine.UI;

public class UI : MonoBehaviour {

    public Vector3 camPos1 = new Vector3(99.4f, 88.5f, 49.7f);
    public Vector3 camPos2 = new Vector3(79.4f, 34.4f, 59.6f);
    public Vector3 camPos3 = new Vector3(125.5f, 34.4f, 43.7f);
    public Vector3 camPos4 = new Vector3(29.0f, 26.4f, 41.8f);

    public Button buttonCamPos1;
    public Button buttonCamPos2;
    public Button buttonCamPos3;
    public Button buttonCamPos4;

    public Button buttonTestProlog1;
    public Button buttonTestProlog2;
    public Button buttonTestProlog3;
    public Button buttonTestProlog4;
    public Button buttonTestProlog5;

    public GameObject PrologTestGO;

    public GameObject Cam;

    public Toggle toggleBoomGates;
    public Toggle toggle;

    public GameObject[] trafficLightsCrossroadA;    

    public InputField[] InputFieldsCrossroadA;

    // Use this for initialization
    void Start () {
    }

    void Awake() {
        toggleBoomGates.onValueChanged.AddListener(boomGateEvent);
        toggle.onValueChanged.AddListener(trafficLightTestToogle);

        buttonCamPos1.onClick.AddListener(camButtonEvent1);
        buttonCamPos2.onClick.AddListener(camButtonEvent2);
        buttonCamPos3.onClick.AddListener(camButtonEvent3);
        buttonCamPos4.onClick.AddListener(camButtonEvent4);

        buttonTestProlog1.onClick.AddListener(testPrologEvent1);
        buttonTestProlog2.onClick.AddListener(testPrologEvent2);
        buttonTestProlog3.onClick.AddListener(testPrologEvent3);
        buttonTestProlog4.onClick.AddListener(testPrologEvent4);
        buttonTestProlog4.onClick.AddListener(testPrologEvent5);
    }
	
	// Update is called once per frame
	void Update () {
	    setTextOfInputField();
	}

    //####################################################################################################
    /// <summary>
    ///  set the text of the inputfield array
    /// </summary>
    private void setTextOfInputField() {

        for (int i = 0; i < trafficLightsCrossroadA.Length; i++) {
            // set text, if array size > i
            if (i < InputFieldsCrossroadA.Length)
                InputFieldsCrossroadA[i].text = trafficLightsCrossroadA[i].GetComponent<TrafficLight>().State.ToString();
        }
    }

    /// <summary>
    /// add options to dropdown
    /// </summary>
    /// <param name="dropdown"></param>
    private void addOptionsToDropDown(Dropdown dropdown) {
        dropdown.options.Clear();

        //add Options to dropdown
        foreach (string state in System.Enum.GetNames(typeof(TrafficLight.States))) {
            dropdown.options.Add(new Dropdown.OptionData() { text = state });
        }        
    }


    /// <summary>
    /// Call function CamMoving script to set postion of cam
    /// </summary>
    /// <param name="postion"></param>
    void setCamPostion(Vector3 postion) {
        Cam.GetComponent<CamMoving>().setCamPosition(postion);
    }

    //########################################## Events  ##########################################################
    void boomGateEvent(bool value) {
        foreach(var boomGate in GameObject.FindGameObjectsWithTag("BoomGate")) {
            boomGate.GetComponent<BoomGate>().isOpen = value;
        }
    }

    void trafficLightTestToogle(bool value) {
        if (value) {
            trafficLightsCrossroadA[0].GetComponent<TrafficLight>().switchToGreen();
        }
        else {
            trafficLightsCrossroadA[0].GetComponent<TrafficLight>().switchToRed();
        }
    }

    void camButtonEvent1() {
        setCamPostion(camPos1);
    }
    void camButtonEvent2() {
        setCamPostion(camPos2);
    }
    void camButtonEvent3() {
        setCamPostion(camPos3);
    }
    void camButtonEvent4() {
        setCamPostion(camPos4);
    }   

    void testPrologEvent1() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState("'keineAktion'");
    }

    void testPrologEvent2() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState("'b3'");
    }

    void testPrologEvent3() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState("'k10'");
    }

    void testPrologEvent4() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState("'fa10'");
    }

    void testPrologEvent5() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState("'k12'");
    }
}
