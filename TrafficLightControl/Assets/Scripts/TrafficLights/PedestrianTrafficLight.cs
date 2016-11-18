using UnityEngine;
using System;

public class PedestrianTrafficLight : TrafficLight, IIntervalMultiplierUpdate {

    public PedestrianButton[] PedestrianTrafficLightButtons;

    public long Interval = 2000;
    public float OffsetAudioSource = 25;

    private GameObject audioSourceHolder;
    private AudioSource audioSource;
    private AudioClip clip1;
    private AudioClip clip2;

    protected override void InitTimerGreen() {
        timerGreen = new Timer
        {
            Interval = Interval,
            AutoReset = false
        };
        timerGreen.Elapsed += timerEventToGreen;

        initAudio();
    }

    /// <summary>
    /// Initialise the audiosource
    /// </summary>
    private void initAudio() {

        //Gameobject to hold the audiosource
        //this gameobject is + Offset over the pedestrian gameobject
        audioSourceHolder = GameObject.CreatePrimitive(PrimitiveType.Sphere);
        audioSourceHolder.GetComponent<MeshRenderer>().enabled = false;
        audioSourceHolder.name = this.name + "_AudioSource";
        audioSourceHolder.transform.position = new Vector3(transform.position.x, transform.position.y + OffsetAudioSource, transform.position.z);

        audioSourceHolder.transform.SetParent(gameObject.transform);

        audioSourceHolder.AddComponent<AudioSource>();

        try {           
            clip1 = Resources.Load("Sound/pedestrian-crossing_1") as AudioClip;
            clip2 = Resources.Load("Sound/pedestrian-crossing_2") as AudioClip;
        }
        catch(Exception e) {
            print("ERROR: " + e);
        }

        if (clip1 != null)
            clip1.LoadAudioData();
        else
            print("clip1 is null");
        if(clip2 != null)
            clip2.LoadAudioData();
        else
            print("clip2 is null");

        audioSource = audioSourceHolder.GetComponent<AudioSource>();
        audioSource.playOnAwake = true;
        audioSource.loop = true;
        audioSource.maxDistance = 8.0f;
        audioSource.minDistance = 0.1f;

        AnimationCurve curve = new AnimationCurve();
        curve.AddKey(0, 1);
        curve.AddKey(7, 0.1f);
        curve.AddKey(8, 0);

        audioSource.SetCustomCurve(AudioSourceCurveType.CustomRolloff, curve);
        audioSource.rolloffMode = AudioRolloffMode.Custom;
        audioSource.clip = clip1;
        audioSource.spatialBlend = 1f;
        audioSource.Play();
    }

    /// <summary>
    /// switch state from red to green
    /// </summary>
    public override void switchToGreen() {
        //only if red or in some sec red
        if (State != States.Green) {
            timerGreen.Start();
        }
    }

    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public override void switchToRed() {
        //only if red or in some sec red
        if (State != States.Red) {
            state = States.Red;
        }
    }

    /// <summary>
    /// switch state and emission
    /// </summary>
    protected override void switchState() {

        if (oldState != State) {
            oldState = State;
            //switch emissioncolor of gameobjects
            switch (State) {
                case States.Red:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);

                    changeAudioClip(true);
                    break;
                case States.Yellow:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Green:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);

                    switchOffPedestrianTrafficLightLights();

                    changeAudioClip(false);
                    break;
                case States.RedAndOrange:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.On:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
            }
        }
    }

    private void changeAudioClip(bool clip) {
        if (clip)
            audioSource.clip = clip1;
        else
            audioSource.clip = clip2;
        audioSource.Play();
    }

    /// <summary>
    /// switch off all PedestrianTrafficLight Lights
    /// </summary>
    private void switchOffPedestrianTrafficLightLights() {
        foreach(var tmp in PedestrianTrafficLightButtons) {
            tmp.switchOffEmission();
        }
    }

    public new void updateMultiplier(float value)
    {
        timerGreen.Interval = (long)(Interval * value);
        audioSource.pitch = 1f / value;
    }
}
