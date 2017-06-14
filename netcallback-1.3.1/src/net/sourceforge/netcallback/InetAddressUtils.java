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

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Static utility methods for java.net.InetAddress handling.
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:52 $
 */
public final class InetAddressUtils {

  /** Prevent instantiation */
  private InetAddressUtils() {
  }

  /**
   * Returns the IP address string "%d.%d.%d.%d" for the given IPv4 address.
   *
   * @param address - a 4-byte array in network byte order 
   *                 (highest order first)
   * @exception IllegalArgumentException - if the array address argument
   *                                       is not of length 4
   */
  public static String getDottedIPv4Address(byte[] address) {
    if (address == null) {
      throw new NullPointerException("null address argument");
    }
    
    if (address.length != 4) {
      throw new IllegalArgumentException("Invalid IPv4 address length " + 
                                         address.length);
    }
    
    return( (address[0] & 0xFF) + "." +
            (address[1] & 0xFF) + "." +
            (address[2] & 0xFF) + "." +
            (address[3] & 0xFF) );
  }

  /**
   * Constructs an InetAddress object for the IP address specified in
   * the network-byte-order array.
   *
   * @param address - a 4-byte array in network byte order 
   *                 (highest order first)
   * @exception IllegalArgumentException - if the array address argument
   *                                       is not of length 4
   */ 
  public static InetAddress getByAddress(byte[] address) {
    try {
      InetAddress inet = InetAddress.getByName
	(getDottedIPv4Address(address));
      return(inet);
    } catch (UnknownHostException e) {
      throw new RuntimeException("Internal error: unexpected exception on " +
				 "dotted IP address parsing: " + 
				 e.getClass().getName() + ": " +
				 e.getMessage());
    }
  }
}
