using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif


[ExecuteInEditMode]
public class SplineModifier : MonoBehaviour
{

    private BezierSpline[] splines;

    void ShowSplines()
    {
        splines = GetComponentsInChildren<BezierSpline>();

        foreach (var spline in splines)
        {
            spline.UpdatePositionsInEditor();
        }
    }


#if UNITY_EDITOR
    // update in editor
    void OnEnable() { EditorApplication.update += ShowSplines; }
    void OnDisable() { EditorApplication.update -= ShowSplines; }
#endif
}
