/// <summary>
/// Marshals events and data between ConsoleController and uGUI.
/// Copyright (c) 2014-2015 Eliot Lash
/// </summary>
using UnityEngine;
using UnityEngine.UI;

public class ConsoleView : MonoBehaviour
{
    ConsoleController console = new ConsoleController();

    bool didShow = false;

    public GameObject viewContainer; //Container for console view, should be a child of this GameObject
    public Text logTextArea;
    public InputField inputField;

    void Start()
    {
        if (console != null)
        {
            console.VisibilityChanged += onVisibilityChanged;
            console.LogChanged += onLogChanged;
        }
        updateLogStr(console.ScrollbackAry);
    }

    ~ConsoleView()
    {
        console.VisibilityChanged -= onVisibilityChanged;
        console.LogChanged -= onLogChanged;
    }

    void Update()
    {
        //Toggle visibility when tilde key pressed
        if (Input.GetKeyUp("^"))
        {
            toggleVisibility();
        }

        //Toggle visibility when 5 fingers touch.
        if (Input.touches.Length == 5)
        {
            if (!didShow)
            {
                toggleVisibility();
                didShow = true;
            }
        }
        else
        {
            didShow = false;
        }
    }

    void toggleVisibility()
    {
        setVisibility(!viewContainer.activeSelf);
    }

    void setVisibility(bool visible)
    {
        viewContainer.SetActive(visible);
    }

    void onVisibilityChanged(bool visible)
    {
        setVisibility(visible);
    }

    void onLogChanged(string[] newLog)
    {
        updateLogStr(newLog);
    }

    void updateLogStr(string[] newLog)
    {
        if (newLog == null)
        {
            logTextArea.text = "";
        }
        else
        {
            logTextArea.text = string.Join("\n", newLog);
        }
    }

    /// <summary>
    /// Event that should be called by anything wanting to submit the current input to the console.
    /// </summary>
    public void runCommand()
    {
        console.Log(inputField.text);
        inputField.text = "";
    }

    public void Log(string message)
    {
        console.Log(message);
    }
}