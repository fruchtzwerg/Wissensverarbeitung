using UnityEngine;

public static class Helper
{
    public static T FindComponentInChildWithTag<T>(this GameObject parent, string tag) where T : Component
    {
        var children = parent.GetComponentsInChildren<T>();

        foreach (var child in children)
        {
            if (child.CompareTag(tag))
                return child;
        }

        return null;
    }
}