package org.heavymeta.vehicles;

import javax.swing.*;


/**
   <p>An object that implements the Renderer interface can be used to
   render a World at an instantaneous moment of the World's time.  The
   Renderer interface is intended to be flexible enough to encompass
   simple 2D graphics, realtime 3D graphics, offline 3D graphics,
   sound, etc.

   <p>The lifetime of a typical Renderer looks like this:

   <ol>
     <p><li>The Renderer is created.

     <p><li>The controlling application calls {@link #getUIComponent()
     getUIComponent} to ask the Renderer for a JComponent containing
     the Renderer's user interface.

     <p><li>The controlling application calls {@link #setWorld(World)
     setWorld} to tell the Renderer the World it will be rendering.
     The Renderer may do some pre-processing at this point to prepare
     for later rendering. Interactive Renderers may choose to render
     the World in its initial state.  Offline Renderers that need
     specialized configuration could pop up a configuration dialog.

     <p><li>The controlling application may make repeated calls to the
     Renderer's {@link #render() render} method.  In between calls to
     render the World is usually taken through simulated steps of
     time.

     <p><li>The controlling application calls the Renderer's {@link
     #finish() finish} method to indicate that the Renderer should
     shutdown.  The Renderer should perform any necessary cleanup now.
     </ol>

   <p>In addition to the lifetime events listed above, the controlling
   application may call other Renderer methods at certain times.

   <ul>
     <p><li>Renderer's with a configuration dialog (that is, their {@link
     #hasSettingsDialog() hasSettingsDialog} method returns true) may
     have their {@link #showSettings(JFrame) showSettings} called at
     any time while render is not being called to indicate that the
     Renderer should pop up a configuration dialog.

     <p><li>The {@link #sync() sync} method may be called at any time
     while render is not being called to indicate that any
     asynchronous processing remaining from the last call to render
     should be completed.  The sync method should block until all such
     processing has finished.

     <p><li>The {@link #cancel() cancel} method may be called while a call
     to render is in progress to indicate that the current rendering
     is being cancelled and should return immediately.

     <p><li>The controlling application may call a Renderer's {@link
     #getName() getName} and {@link #getDescription() getDescription}
     methods at any time to get information about the Renderer.  </ul>
**/

public interface Renderer {
    public abstract JComponent getUIComponent();
    public abstract void setWorld(World w);
    public abstract void render();
    public abstract void sync();
    public abstract void cancel();
    public abstract boolean hasSettingsDialog();
    public abstract void showSettingsDialog(JFrame f);
    public abstract void finish();
    public abstract String getName();
    public abstract String getDescription();
}
