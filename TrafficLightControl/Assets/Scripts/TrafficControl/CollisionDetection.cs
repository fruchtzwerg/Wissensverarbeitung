using System;
using System.Collections.Generic;
using UnityEngine;
using UnityThreading;

public class CollisionDetection : MonoBehaviour
{

    public const string TAG_LIGHT = "Light";
    public const string TAG_VEHICLE = "Vehicle";
    public const string TAG_COL_FRONT = "Collider_Front";
    public const string TAG_COL_REAR = "Collider_Rear";
    
    private SplineWalker _walker;

    private void Start()
    {
        _walker = GetComponentInParent<SplineWalker>();
    }

    private void OnTriggerEnter(Collider other)
    {
        // front collision with light
        if (CompareTag(TAG_COL_FRONT) && other.CompareTag(TAG_LIGHT))
        {
            _walker.Move = false;
            return;
        }

        // front collision with rear of ohter
        if (CompareTag(TAG_COL_FRONT) && other.CompareTag(TAG_COL_REAR))
        {
            _walker.Move = false;
        }
    }

    private void OnTriggerStay(Collider other)
    {
        var waypoint = other.GetComponent<SplineWaypoint>();
        if (waypoint == null)
            return;

        waypoint.IsOccupied = true;
    }

    private void OnTriggerExit(Collider other)
    {
        // front collision with light
        if (CompareTag(TAG_COL_FRONT) && other.CompareTag(TAG_LIGHT))
        {
            _walker.Move = true;
            return;
        }

        // front collision with rear of other
        if (CompareTag(TAG_COL_FRONT) && other.CompareTag(TAG_COL_REAR))
        {
            _walker.Move = true;
            return;
        }

        // rear collider exits spawn area
        if (CompareTag(TAG_COL_REAR))
        {
            var waypoint = other.GetComponent<SplineWaypoint>();
            if (waypoint == null)
                return;

            waypoint.IsOccupied = false;
        }
    }
}