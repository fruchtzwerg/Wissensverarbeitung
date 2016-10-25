using UnityEngine;
using System.Collections;

public class TrafficLight : MonoBehaviour {
    


    public GameObject RedLight;
    public GameObject OrangeLight;
    public GameObject GreenLight;


    public Shader shader;
    
    private Renderer rendRed;
    private Renderer rendOrange;
    private Renderer rendGreen;

    private Color black = new Color(0, 0, 0);
    private Color red = new Color(1f, 0, 0);
    private Color orange = new Color(1f, 1f, 0);
    private Color green = new Color(0, 1f, 0);

    public States state = States.off;
    private States oldState = States.off;
    public enum States {
        red,
        orange,
        green,
        redAndOrange,
        greenAndOrange,
        off,
        on
    }

    // Use this for initialization
    void Start () {

        //init Rendere with different materials
        rendRed = new Renderer();         

        rendRed = RedLight.GetComponent<Renderer>();
        rendRed.material = new Material(shader);
        rendRed.material.color = red;

        rendOrange = OrangeLight.GetComponent<Renderer>();
        rendOrange.material = new Material(shader);
        rendOrange.material.color = orange;

        rendGreen = GreenLight.GetComponent<Renderer>();
        rendGreen.material = new Material(shader);
        rendGreen.material.color = green;
    }
	
	// Update is called once per frame
	void Update () {
        switchOnLight();
	}

    void switchOnLight() {

        if (oldState != state) {
            oldState = state;
            //switch emissioncolor of gameobjects
            switch (state) {
                case States.red:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.orange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.green:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                case States.greenAndOrange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                case States.redAndOrange:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.on:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
            }

        }
        
    }
}
