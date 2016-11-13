using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

[ExecuteInEditMode]
public class SplineWaypoint : MonoBehaviour
{

    public SplineWaypoint NextWaypoint;
    public BezierSpline Spline;

    public bool IsDestination;
    public bool IsOrigin;

    private void CreateSpline()
    {
        if (NextWaypoint == null || Spline != null) return;

        Spline = gameObject.AddComponent<BezierSpline>();
        Spline.StartPoint = transform;
        Spline.EndPoint = NextWaypoint.transform;
    }

#if UNITY_EDITOR
    // update in editor
    void OnEnable() { EditorApplication.update += CreateSpline; }
    void OnDisable() { EditorApplication.update -= CreateSpline; }
#endif
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
