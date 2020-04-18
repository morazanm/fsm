# Graphviz Installation Instructions

### Windows 10

A Youtube video how how to install can be found [here](https://www.youtube.com/watch?v=WkLhdBbf-3E)

1) Download **Graphviz Stable version** at: https://graphviz.gitlab.io/download/
    - Choose the .msi option!
2) Open the installer and follow the installation instructions
    - <span style="color:red">Remember the directory that you       installed Graphviz in!
    </span>
3) In the bottom left of the screen type *environment* in the search bar
4) Click on "Edit the system environment variables"
5) In the *Advanced tab* click the *Environment Variables* button
6) Click on **PATH** so that it is highlighted, then click *Edit*
7) Then click **New** and add the location to where you installed graphviz
    - <span style="color:red">Add the /bin file inside of the graphviz file!</span>
8) Press **OK**

If the above steps were followed correctly, open a command prompt and type the following:

```powershell
> dot -V
dot - graphviz version 2.38.0 (20140413.2041)
```

### Mac
Coming Soon!

### Linux
Coming Soon!