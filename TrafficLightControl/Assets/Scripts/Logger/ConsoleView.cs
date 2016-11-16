/// <summary>
/// Marshals events and data between ConsoleController and uGUI.
/// Copyright (c) 2014-2015 Eliot Lash
/// </summary>

using System;
using UnityEngine;
using UnityEngine.UI;

public class ConsoleView : MonoBehaviour
{
    ConsoleController _console = new ConsoleController();

    bool _didShow = false;

    public GameObject ViewContainer; //Container for console view, should be a child of this GameObject
    public Text LogTextArea;
    public EventProcessor Processor;


    void Start()
    {
        if (_console != null)
        {
            _console.VisibilityChanged += OnVisibilityChanged;
            _console.LogChanged += OnLogChanged;
        }
        //if (Logger != null) Logger.Log += LogMessage;
        UpdateLogStr(_console.ScrollbackAry);

        if(Processor == null)
        Processor = gameObject.AddComponent<EventProcessor>();
    }

    ~ConsoleView()
    {
        _console.VisibilityChanged -= OnVisibilityChanged;
        _console.LogChanged -= OnLogChanged;
    }

    void Update()
    {
        //Toggle visibility when tilde key pressed
        if (Input.GetKeyUp("^"))
        {
            ToggleVisibility();
        }

        //Toggle visibility when 5 fingers touch.
        if (Input.touches.Length == 5)
        {
            if (!_didShow)
            {
                ToggleVisibility();
                _didShow = true;
            }
        }
        else
        {
            _didShow = false;
        }
    }

    void ToggleVisibility()
    {
        SetVisibility(!ViewContainer.activeSelf);
    }

    void SetVisibility(bool visible)
    {
        ViewContainer.SetActive(visible);
    }

    void OnVisibilityChanged(bool visible)
    {
        SetVisibility(visible);
    }

    void OnLogChanged(string[] newLog)
    {
        UpdateLogStr(newLog);
    }

    void UpdateLogStr(string[] newLog)
    {
        if (newLog == null)
        {
            LogTextArea.text = "";
        }
        else
        {
            LogTextArea.text = string.Join("\n", newLog);
        }
    }

    public void LogMessage(string message)
    {
        _console.Log(message);
    }
}