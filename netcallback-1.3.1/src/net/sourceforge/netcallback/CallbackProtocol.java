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

/**
 * Callback protocol constants.
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:52 $
 */
public final class CallbackProtocol {

  /**
   * Current network callback version
   */
  public static final int VERSION = 2;

  /**
   * Request passed from the private to the public server followed by
   * the version number and the redirect port number.
   */
  public static final int CONNECT = 1;
  
  /**
   * Request a callback followed by one argument specifying the port
   */
  public static final int CALLBACK = 2;

  /**
   * Request the termination of the tunnel followed by one argument
   * specifying the port to be terminated
   */
  public static final int CLOSE    = 3;

  /**
   * Ping the host (no args)
   */
  public static final int PING     = 4;

  /**
   * UDP datagram
   */
  public static final int DATAGRAM = 5;

  /**
   * Returns a description of the protocol request
   */
  public static String getOperationDescription(int op) {
    switch(op) {
    case CONNECT:  return("CONNECT");
    case CALLBACK: return("CALLBACK");
    case CLOSE:    return("CLOSE");
    case PING:     return("PING");
    case DATAGRAM: return("DATAGRAM");
    default:       return("UNKNOWN(" + op + ")");
    }
  }
}
