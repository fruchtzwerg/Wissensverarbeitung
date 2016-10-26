using UnityEngine;
using System.Collections;

public class CamMoving : MonoBehaviour {

    public float maxLeft;
    public float maxRight;
    public float maxForward;
    public float maxBackward;
    public float maxZoomIn = 16.0f;
    public float maxZoomOut = 88.5f;

    public float zoomSpeed = 0.8f;
    public float movingSpeed = 0.3f;

	// Use this for initialization
	void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
        if (Input.GetKey(KeyCode.W)) {
            moveForward();
        }
        if (Input.GetKey(KeyCode.S)) {
            moveForward(false);
        }
        if (Input.GetKey(KeyCode.D)) {
            moveRight();
        }
        if (Input.GetKey(KeyCode.A)) {
            moveRight(false);
        }
        if(Input.GetAxis("Mouse ScrollWheel") < 0) {
            zoomIn();
        }
        if (Input.GetAxis("Mouse ScrollWheel") > 0) {
            zoomIn(false);
        }
    }

    /// <summary>
    /// Translate this object in z/-z direction
    /// </summary>
    /// <param name="forward"></param>
    void moveForward(bool forward = true) {

        float speed = movingSpeed;

        if (!forward)
            speed *= -1;

        transform.position = new Vector3(transform.position.x, transform.position.y, transform.position.z + speed);
    }

    /// <summary>
    /// Translate this object in x/-x direction
    /// </summary>
    /// <param name="forward"></param>
    void moveRight(bool right = true) {

        float speed = movingSpeed;

        if (!right)
            speed *= -1;

        transform.position = new Vector3(transform.position.x + speed, transform.position.y, transform.position.z );
    }

    /// <summary>
    /// Translate this object in y/-y direction
    /// </summary>
    /// <param name="zoomIn"></param>
    void zoomIn(bool zoomIn = true) {
        float speed = zoomSpeed;

        if (!zoomIn)
            speed *= -1;

        Vector3 posTmp = transform.position;
        Vector3 newPos = posTmp;
        
        newPos = new Vector3(posTmp.x, posTmp.y + speed, posTmp.z);
        
        //limit zoom
        if (newPos.y >= (maxZoomIn) && newPos.y <= (maxZoomOut + zoomSpeed))
            transform.position = newPos;
    }

    /// <summary>
    /// set the postion of the camera
    /// </summary>
    /// <param name="position"></param>
    public void setCamPosition(Vector3 position) {
        transform.position = position;
    }
}
