using System;
using UnityEngine;

public class SplineDecorator : MonoBehaviour
{

    public BezierSpline Spline;
    public int Frequency;
    public bool LookForward;
    public Transform[] Items;

    private void Awake()
    {
        if (Frequency <= 0 || Items == null || Items.Length == 0)
        {
            return;
        }
        float stepSize = Frequency * Items.Length;
        if (Spline.Loop || Math.Abs(stepSize - 1) < .0001f)
        {
            stepSize = 1f / stepSize;
        }
        else
        {
            stepSize = 1f / (stepSize - 1);
        }
        for (int p = 0, f = 0; f < Frequency; f++)
        {
            for (var i = 0; i < Items.Length; i++, p++)
            {
                var item = Instantiate(Items[i]);
                var position = Spline.GetPoint(p * stepSize);
                item.transform.localPosition = position;
                if (LookForward)
                {
                    item.transform.LookAt(position + Spline.GetDirection(p * stepSize));
                }
                item.transform.parent = transform;
            }
        }
    }
}