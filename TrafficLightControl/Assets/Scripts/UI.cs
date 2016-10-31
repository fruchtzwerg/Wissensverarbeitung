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

    public Button buttonTestProlog;
    public GameObject PrologTestGO;

    public GameObject Cam;

    public Toggle toggleBoomGates;
    public Toggle toggle;

    public GameObject[] trafficLights;    

    public InputField[] InputFields;

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

        buttonTestProlog.onClick.AddListener(testPrologEvent);
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

        for (int i = 0; i < trafficLights.Length; i++) {
            // set text, if array size > i
            if (i < InputFields.Length)
                InputFields[i].text = trafficLights[i].GetComponent<TrafficLight>().state.ToString();
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
            trafficLights[0].GetComponent<TrafficLight>().switchToGreen();
        }
        else {
            trafficLights[0].GetComponent<TrafficLight>().switchToRed();
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

    void testPrologEvent() {
        PrologTestGO.GetComponent<TrafficLightControl>().NextState();
    }
}
