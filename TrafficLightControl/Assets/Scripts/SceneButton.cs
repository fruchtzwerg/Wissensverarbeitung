using UnityEngine;
using System.Collections;

public class SceneButton : MonoBehaviour {

    public GameObject TrafficLightGO;
    public GameObject PushedLight;
    public GameObject PartnerButton;

    public Shader shader;
    private Renderer rendRed;

    private bool isPushed;

    private Color black = new Color(0, 0, 0);
    private Color red = new Color(1f, 0, 0);

    // Use this for initialization
    void Start () {

        rendRed = new Renderer();

        rendRed = PushedLight.GetComponent<Renderer>();
        rendRed.material = new Material(shader);
        rendRed.material.EnableKeyword("_EMISSION");
        rendRed.material.color = red;
    }
	
	// Update is called once per frame
	void Update () {
	    //if(this.TrafficLightGO.GetComponent<TrafficLight>().State == TrafficLight.States.green && isPushed) {
        //    rendRed.material.SetColor("_EmissionColor", black);
        //}
	}

    void OnMouseDown() {

        pushed();

        if(PartnerButton != null) {
            PartnerButton.GetComponent<SceneButton>().partnerButtonPushed();
        }
    }

    private void pushed() {
        isPushed = true;

        rendRed.material.SetColor("_EmissionColor", red);
    }

    public void partnerButtonPushed() {
        pushed();
    }

    public void switchOffEmission() {
        isPushed = false;

        rendRed.material.SetColor("_EmissionColor", black);
    }
}
