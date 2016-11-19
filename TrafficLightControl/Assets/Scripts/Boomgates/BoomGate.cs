﻿using UnityEngine;
using System.Collections;

public class BoomGate : TrafficLight {

    public GameObject pivot;
    public bool isOpen = false;
    private bool isEnable = false;
    private bool isEnableOld = false;

    private Vector3 pivotVector3;
    private Quaternion openPosition; 
    private Quaternion closePosition;

    private int degreeCounter = 0;

    private float multiplier = 1f;

    // Use this for initialization
    void Start () {
        state = States.Closed;
    }
	
	// Update is called once per frame
	void Update () {
        rotatePivot(isOpen);
        
        if(isEnable != isEnableOld && _collider != null) {
            isEnableOld = isEnable;

            EnableCollider(isEnable);
        }
	}

    public void setIsOpen(bool _isOpen) {
        isOpen = _isOpen;
        isEnable = _isOpen;     
    }

    private void rotatePivot(bool open) {        
                
        if (state == States.Closed && open) {

            state = States.Opening;
        }
        //State opening -> opening barrier
        else if(state == States.Opening && open) {
            pivot.transform.Rotate(0,1 * multiplier, 0);

            degreeCounter++;

            if (degreeCounter == 90) {
                degreeCounter = 0;

                state = States.Open;
            }
        }
        //State closing -> closing barrier
        else if (state == States.Closing && !open) {
            pivot.transform.Rotate(0, -1* multiplier, 0);

            degreeCounter++;

            if(degreeCounter == 90) {
                degreeCounter = 0;

                state = States.Closed;
            }
        }
        else if(state == States.Open && !open) {
            pivot.transform.Rotate(-1,0,0);
            state = States.Closing;
        }
    }
    public new void updateMultiplier(float value) {
        multiplier = value;
    }
}
