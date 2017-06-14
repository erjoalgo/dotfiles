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

import java.io.*;

/**
 * Periodically generates CallbackProtocol.PING requests.
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:53 $
 */
public class PingThread extends Thread {
  /** Data output stream used to generate ping requests */
  protected DataOutputStream ostream;

  /** Number of milliseconds to sleep between pings */
  protected int pingSleep = 30000;

  /**
   * Constructs a new ping thread that issues an CallbackProtocol.PING
   * request on a regular interval.
   * 
   * @param ostream - data output stream used to generate request (access
   *                  synchronized)
   */
  public PingThread(DataOutputStream ostream) {
    this.ostream = ostream;
  }

  /**
   * Loops while no I/O errors have occured, generating ping requests every
   * pingSleep milliseconds.
   */
  public void run() {
    try {
      while(true) {

	Thread.sleep(pingSleep);

	synchronized(ostream) {
	  ostream.writeInt(CallbackProtocol.PING);
	  ostream.flush();
	}
      }
    } catch (Throwable e) {
    }
  }
}
