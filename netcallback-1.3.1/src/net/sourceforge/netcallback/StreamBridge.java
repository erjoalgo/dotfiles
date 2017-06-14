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
 * Asynchronous bridging of traffic between an input and and output stream.
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:55 $
 */
public class StreamBridge extends Thread {

  /** byte input stream */
  protected final InputStream istream;

  /** byte output stream */
  protected final OutputStream ostream;

  /** true if the bridge is active, false if it should terminate */
  protected boolean active;
  /**
   * Constructs a new asynchronous bridge by spawning a thread copying
   * traffic from the input stream to the output stream.
   * <p>
   * When the source of the input stream is a network socket, and in order to
   * detect the end of stream expiration in a timely manner, the socket
   * timeout must be set to a reasonable value (e.g. 1 second)
   *
   * @param istream - the input stream
   * @param ostream - the output stream
   * @param description - a string describing the connection (used to name the
   *                      thread)
   */
  public StreamBridge(InputStream istream,
		      OutputStream ostream,
		      String description) {

    super(description);

    if (istream == null) 
      throw new NullPointerException("null istream argument");

    if (ostream == null) 
      throw new NullPointerException("null ostream argument");

    this.istream = istream;
    this.ostream = ostream;

    active = true;

    start();
  }

  /**
   * Request asynchronous termination of the stream bridge 
   */
  public void terminate() {
    Log.debug("terminate() invoked on " + getName());
    active = false;
    interrupt();
  }

  /**
   * Thread copying bytes from the input stream and writing them to the
   * output stream until the end of file.
   */
  public void run() {

    try {
      byte[] buffer = new byte[4096];
      int size = 0;
      
      while ( (active) && (size != -1) ) {
	if (size > 0) {
	  ostream.write(buffer, 0, size);
	  ostream.flush();
	}
      
	try {
	  size = istream.read(buffer);
	} catch (InterruptedIOException e) {
	  size = e.bytesTransferred;
	}
      }
    } catch (IOException e) {
      // expected
      Log.debug(getName() + ": " + e.getClass().getName() + ": " + e.getMessage());
    } catch (Throwable e) {
      // unexpected
      e.printStackTrace();
    } finally {
      try {
	Log.debug("Closing ostream " + getName());
	ostream.close();
      } catch (Throwable e) { e.printStackTrace(); }

      try {
	Log.debug("Closing istream " + getName());
	istream.close();
      } catch (Throwable e) { e.printStackTrace(); }
    }
  }
}
