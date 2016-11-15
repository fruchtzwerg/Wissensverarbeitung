using UnityEngine;
using System.Collections;
using UnityStandardAssets.CrossPlatformInput;

public class CamSwitcher : MonoBehaviour
{

    public GameObject[] Cameras;
    private static int index;

    // Update is called once per frame
    void Update()
    {
        if (Cameras == null)
            return;

        if (Input.GetButtonUp("SwitchCam"))
            NextCamera();
    }

    private void NextCamera()
    {
        print(index);
        // disable currently active camera
        Cameras[index].SetActive(false);

        // increase index to next camera or back to 0
        if (++index >= Cameras.Length)
            index = 0;

        print(index);
        // enable next camera
        Cameras[index].SetActive(true);
    }
}
