using UnityEngine;
using System.Collections;
using UnityEngine.UI;

public class UI : MonoBehaviour {

    public Toggle toggle;

    public GameObject[] trafficLights;    

    public InputField[] InputFields;

    // Use this for initialization
    void Start () {
    }

    void Awake() {
        toggle.onValueChanged.AddListener(boomGateEvent);        
    }
	
	// Update is called once per frame
	void Update () {
	    setTextOfInputField();
	}

    //####################################################################################################

    private void setTextOfInputField() {

        for(int i = 0; i<trafficLights.Length; i++) {
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

    //####################################################################################################
    void boomGateEvent(bool value) {
        foreach(var boomGate in GameObject.FindGameObjectsWithTag("BoomGate")) {
            boomGate.GetComponent<BoomGate>().isOpen = value;
        }
    }
}
