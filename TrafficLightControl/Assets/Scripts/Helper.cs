using System.Linq;
using UnityEngine;
using UnityEngine.UI;

public static class Helper
{
    public static T FindComponentInChildWithTag<T>(this GameObject parent, string tag) where T : Component
    {
        var children = parent.GetComponentsInChildren<T>();

        return children.FirstOrDefault(child => child.CompareTag(tag));
    }

    public static void ChangeAlpha(this Selectable button, int alpha)
    {
        var block = button.colors;
        var color = block.normalColor;

        color.a = alpha / 256f;

        block.normalColor = color;
        button.colors = block;
    }
    public static void ChangeAlpha(this Text text, int alpha)
    {
        var color = text.color;

        color.a = alpha / 256f;
        
        text.color = color;
    }
}