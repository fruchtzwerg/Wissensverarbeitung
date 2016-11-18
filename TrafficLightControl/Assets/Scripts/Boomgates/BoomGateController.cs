using UnityEngine;
using System.Collections;
using System;

public class BoomGateController : TrafficLight, IIntervalMultiplierUpdate {

    public BoomGate[] boomgates;

    public Vector3 AudioSourcePostion = new Vector3(26.232f, 31.86f, 31.58f);

    private GameObject audioSourceHolder;
    private AudioSource audioSource;
    private AudioClip clip1;

    // Use this for initialization
    void Start() {
        state = States.Closed;

        initAudio();
    }

    // Update is called once per frame
    void Update() {
        if(boomgates[0]) {
            if(state != boomgates[0].State) {
                state = boomgates[0].State;

                //Play sound
                if (state != States.Open)
                    audioSource.Play();
                else
                    audioSource.Stop();
            }
        }
    }


    public override void switchToGreen() {
        foreach(var tmp in boomgates) {
            tmp.setIsOpen(true);
        }
    }

    public override void switchToRed() {
        foreach (var tmp in boomgates) {
            tmp.setIsOpen(false);
        }
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
        audioSourceHolder.transform.position = AudioSourcePostion;

        audioSourceHolder.transform.SetParent(gameObject.transform);

        audioSourceHolder.AddComponent<AudioSource>();

        try {
            clip1 = Resources.Load("/Sound/train_crossing") as AudioClip;
        }
        catch (Exception e) {
            print("ERROR: " + e);
        }

        if (clip1 != null)
            clip1.LoadAudioData();
        else
            print("clip1 is null");       

        audioSource = audioSourceHolder.GetComponent<AudioSource>();
        audioSource.playOnAwake = true;
        audioSource.loop = true;
        audioSource.maxDistance = 10.0f;
        audioSource.minDistance = 0.3f;

        AnimationCurve curve = new AnimationCurve();
        curve.AddKey(0, 1);
        curve.AddKey(8, 0.1f);
        curve.AddKey(10, 0);

        audioSource.SetCustomCurve(AudioSourceCurveType.CustomRolloff, curve);
        audioSource.rolloffMode = AudioRolloffMode.Custom;
        audioSource.clip = clip1;
        audioSource.spatialBlend = 1f;
        audioSource.Play();        
    }

    public new void updateMultiplier(float value) {
        audioSource.pitch = 1f/value;
    }
}
