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

import java.io.IOException;
import java.net.*;

/**
 * Factory interface implemented by classes creating Socket and ServerSocket
 * instances.
 *
 * @see java.net.Socket
 * @see java.net.ServerSocket
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:52 $
 */
public interface CallbackSocketFactory {
  
  /**
   * Creates a socket to the target address and port
   */
  Socket createSocket(InetAddress address, int port)
    throws IOException;
  
  /**
   * Creates a socket to the target host and port
   */
  Socket createSocket(String host, int port)
    throws UnknownHostException, IOException;

  /**
   * Creates a server socket to the target host and port
   */
  ServerSocket createServerSocket(int port)
    throws IOException;

  /**
   * Returns true if the connection is encrypted, false otherwise
   */
  boolean isSecure();
}
