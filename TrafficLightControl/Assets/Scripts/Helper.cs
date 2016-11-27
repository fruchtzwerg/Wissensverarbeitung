using System.Linq;
using UnityEngine;

public static class Helper
{
    public static T FindComponentInChildWithTag<T>(this GameObject parent, string tag) where T : Component
    {
        var children = parent.GetComponentsInChildren<T>();

        return children.FirstOrDefault(child => child.CompareTag(tag));
    }
}