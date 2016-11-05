using UnityEngine;
using UnityEditor;

[ExecuteInEditMode]
public class SplineWaypoint : MonoBehaviour
{

    public SplineWaypoint NextWaypoint;
    [SerializeField]
    private BezierSpline spline;

    public bool IsDestination;
    public bool IsOrigin;

    void CreateSpline()
    {
        if (NextWaypoint != null && spline == null)
        {
            spline = gameObject.AddComponent<BezierSpline>();
            spline.StartPoint = transform;
            spline.EndPoint = NextWaypoint.transform;
        }
    }

    // update in editor
    void OnEnable() { EditorApplication.update += CreateSpline; }
    void OnDisable() { EditorApplication.update -= CreateSpline; }
}





//using System;
//using UnityEngine;
//using UnityEditor;

//[ExecuteInEditMode]
//public class SplineWaypoint : MonoBehaviour
//{

//    public SplineWaypoint[] NextWaypoints;
//    public BezierSpline[] splines;

//    void CreateSpline()
//    {
//        if (NextWaypoints != null)
//            for (int i = 0; i < NextWaypoints.Length; i++)
//            {
//                if (NextWaypoints[i] != null && splines[i] == null)
//                {
//                    splines[i] = gameObject.AddComponent<BezierSpline>();
//                    splines[i].StartPoint = transform;
//                    splines[i].EndPoint = NextWaypoints[i].transform;
//                }
//            }
//    }

//    update in editor
//    void OnEnable() { EditorApplication.update += CreateSpline; }
//    void OnDisable() { EditorApplication.update -= CreateSpline; }
//}
