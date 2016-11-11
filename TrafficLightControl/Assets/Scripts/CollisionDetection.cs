using UnityEngine;
using System.Collections;

public class CollisionDetection : MonoBehaviour {

    public const string TAG_LIGHT = "Light";
    public const string TAG_VEHICLE = "Vehicle";
    public const string TAG_COL_FRONT = "Collider_Front";
    public const string TAG_COL_REAR = "Collider_Rear";

    private SplineWalker walker;

    void Start()
    {
        walker = GetComponentInParent<SplineWalker>();
    }

    void OnTriggerEnter(Collider col)
    {
        // front collision with light
        if (CompareTag(TAG_COL_FRONT) && col.CompareTag(TAG_LIGHT))
        {
            walker.Move = false;
        }

        // front collision with rear of col
        if (CompareTag(TAG_COL_FRONT) && col.CompareTag(TAG_COL_REAR))
        {
            walker.Move = false;
        }
    }

    void OnTriggerExit(Collider col)
    {
        // front collision with light
        if (CompareTag(TAG_COL_FRONT) && col.CompareTag(TAG_LIGHT))
        {
            walker.Move = true;
        }

        // front collision with rear of col
        if (CompareTag(TAG_COL_FRONT) && col.CompareTag(TAG_COL_REAR))
        {
            walker.Move = true;
        }
    }
}
