/*
 * NetCallback - forwarding TCP ports behind a firewall
 * Copyright (C) 2001 Alexander V. Konstantinou <akonstan@acm.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 */

package net.sourceforge.netcallback;

import java.net.*;
import java.io.*;

/**
 * Asynchronous bridging of traffic between an two sockets (cross-connect)
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:55 $
 */
public class SocketBridge extends Thread {
  
  /** socket1 */
  protected Socket s1;

  /** socket2 */
  protected Socket s2;
  
  /** socket1 -&lt; socket2 stream bridge */
  protected StreamBridge s1ToS2Bridge;

  /** socket2 -&lt; socket1 stream bridge */
  protected StreamBridge s2ToS1Bridge;

  /** bridge description (used to report connection termination */
  protected String description;

  /** optional socket bridge event listener */
  protected SocketBridgeListener listener;

  /**
   * Constructs a new socket bridge asynchronously copying traffic from
   * s1 to s2 and s2 to s1.
   * <p>
   * The sockets are configured to timeout after 1000msec in order
   * to detect the end of file condition.  Then two StreamBridge instances
   * are generated to perform the actual copying.  The main thread waits
   * for the copying threads to terminate, and then closes the sockets.
   *
   * @exception IOException - if one of the sockets cannot be configured
   *                          or an input or output stream cannot be
   *                          opened.
   */
  public SocketBridge(Socket s1, Socket s2) throws IOException {
    this(s1, s2, null);
  }

  /**
   * Constructs a new socket bridge asynchronously copying traffic from
   * s1 to s2 and s2 to s1.
   * <p>
   * The sockets are configured to timeout after 1000msec in order
   * to detect the end of file condition.  Then two StreamBridge instances
   * are generated to perform the actual copying.  The main thread waits
   * for the copying threads to terminate, and then closes the sockets.
   *
   * @exception IOException - if one of the sockets cannot be configured
   *                          or an input or output stream cannot be
   *                          opened.
   */
  public SocketBridge(Socket s1, Socket s2, SocketBridgeListener l)
    throws IOException {

    this.s1 = s1;
    this.s2 = s2;
    this.listener = l;

    description = s1.getInetAddress() + ":" + s1.getPort() + " <-> " +
      s2.getInetAddress() + ":" + s2.getPort();

    setName("SocketBridge[" + description + "]");

    s1ToS2Bridge = 
      new StreamBridge(s1.getInputStream(), s2.getOutputStream(),
		       s1.getInetAddress() + ":" + s1.getPort() + " -> " +
		       s2.getInetAddress() + ":" + s2.getPort());
    
    s2ToS1Bridge =
      new StreamBridge(s2.getInputStream(), s1.getOutputStream(),
		       s2.getInetAddress() + ":" + s2.getPort() + " -> " +
		       s1.getInetAddress() + ":" + s1.getPort());

    start();
  }

  /**
   * Request asynchronous termination of the socket bridge 
   */
  public void terminate() {
    s1ToS2Bridge.terminate();
    s2ToS1Bridge.terminate();
  }

  /**
   * Wait for the copying threads to terminate and then close the sockets.
   */
  public void run() {
    try { 
      s1ToS2Bridge.join();
      s2ToS1Bridge.join();
    } catch (Throwable e) {
      e.printStackTrace();
    }

    try {
      Log.debug("Closing socket " + s1);
      s1.close();
    } catch (Throwable e) {
      e.printStackTrace();
    }

    try {
      Log.debug("Closing socket " + s2);
      s2.close();
    } catch (Throwable e) {
      e.printStackTrace();
    }

    if (listener != null) {
      try {
	listener.socketBridgeClosed(this);
      } catch(Throwable e) {
	e.printStackTrace();
      }
    }
    
    Log.log("Closed " + description);
  }
}
