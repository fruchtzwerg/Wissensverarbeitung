using UnityEngine;
using UnityStandardAssets.CrossPlatformInput;

public class CamMoving : MonoBehaviour {

    public Transform MaxLeft;
    public Transform MaxRight;
    public Transform MaxForward;
    public Transform MaxBackward;
    public float MaxZoomIn = 16.0f;
    public float MaxZoomOut = 88.5f;

    public float ZoomSpeed = 2f;
    public float MovementSpeed = 0.5f;
    public float FastMovementSpeed = 2.5f;
    
	
	// Update is called once per frame
	void Update () {
        Move();
    }

    /// <summary>
    /// Translate this object in z/-z direction
    /// </summary>
    private void Move()
    {
        var hor = CrossPlatformInputManager.GetAxis("Horizontal");
        var vert = CrossPlatformInputManager.GetAxis("Vertical");
        var lat = CrossPlatformInputManager.GetAxis("Mouse ScrollWheel")*ZoomSpeed;
        var latJoy = CrossPlatformInputManager.GetAxis("Joy Y") * ZoomSpeed;
        var goFast = CrossPlatformInputManager.GetButton("Sprint");
        var goFastJoy = CrossPlatformInputManager.GetAxis("Joy Z");
        var speed = goFast || goFastJoy > 0 ? FastMovementSpeed : MovementSpeed;

        var atUpperBounds = transform.position.z + vert >= MaxForward.position.z;
        var atLowerBounds = transform.position.z + vert <= MaxBackward.position.z;
        var atLeftBounds = transform.position.x + hor >= MaxRight.position.x;
        var atRightBounds = transform.position.x + hor <= MaxLeft.position.x;
        var atOuterBounds = transform.position.y - lat + latJoy >= MaxZoomOut;
        var atInnerBounds = transform.position.y - lat + latJoy <= MaxZoomIn;

        if ((atUpperBounds && vert > 0) || (atLowerBounds && vert < 0))
            vert = 0;
        if ((atLeftBounds && hor > 0) || (atRightBounds && hor < 0))
            hor = 0;
        if ((atOuterBounds && lat + latJoy < 0 ) || (atInnerBounds && lat + latJoy > 0))
            lat = latJoy = 0;

        var dir = new Vector3(hor, vert, (lat + latJoy)*ZoomSpeed);
        transform.Translate(dir * speed);
    }



    /// <summary>
    /// set the postion of the camera
    /// </summary>
    /// <param name="position"></param>
    public void SetCamPosition(Vector3 position) {
        transform.position = position;
    }
}
