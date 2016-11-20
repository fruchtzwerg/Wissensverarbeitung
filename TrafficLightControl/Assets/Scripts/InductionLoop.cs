using UnityEngine;
using System.Collections;

public class InductionLoop : MonoBehaviour {

    private BoxCollider _collider;

    public TrafficLightControl trafficLightControl;
    public EventTrigger.Events triggerEvent;

    private Vector3 offset = new Vector3(0, -100, 0);

	// Use this for initialization
	void Start () {
        _collider = GetComponent<BoxCollider>();        
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    void OnTriggerEnter(Collider other) {
        trafficLightControl.EventWasTriggered(triggerEvent.ToString());
    }

    public void enableEventCollider(bool enable) {
        var offset = new Vector3(0, 100, 0);

        if (enable)
            _collider.center += offset;
        else
            _collider.center -= offset;
    }
}
