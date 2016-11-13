using UnityEngine;
using System.Collections;

public class PedestrianButton : MonoBehaviour {

    public TrafficLightControl TrafficLightControl;
    public Renderer PushedLight;
    public PedestrianButton PartnerButton;
    public EventTrigger.Events Event;

    public Shader shader;
    
    // Use this for initialization
    void Start ()
    {
        PushedLight.material = new Material(shader);
        PushedLight.material.EnableKeyword("_EMISSION");
        PushedLight.material.color = Color.red;
    }
	
	// Update is called once per frame
	void Update () {
	    //if(this.TrafficLightGO.GetComponent<TrafficLight>().State == TrafficLight.States.green && isPushed) {
        //    rendRed.material.SetColor("_EmissionColor", black);
        //}
	}

    void OnMouseDown() {
        print("down");
        pushed();

        if(PartnerButton)
            PartnerButton.partnerButtonPushed();

        TrafficLightControl.EventWasTriggered(Event.ToString());
    }

    private void pushed() {
        PushedLight.material.SetColor("_EmissionColor", Color.green);
    }

    public void partnerButtonPushed() {
        pushed();
    }

    public void switchOffEmission() {
        PushedLight.material.SetColor("_EmissionColor", Color.black);
    }
}
