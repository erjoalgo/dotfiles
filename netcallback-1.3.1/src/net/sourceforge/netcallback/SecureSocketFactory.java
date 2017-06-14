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

import javax.net.ssl.*;

/**
 * Factory object creating instances of javax.net.ssl.SSLSocket and
 * javax.net.ssl.SSLServerSocket.
 *
 * @see javax.net.ssl.SSLSocket
 * @see javax.net.ssl.SSLServerSocket
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:55 $
 */
public class SecureSocketFactory implements CallbackSocketFactory {
  
  /** */
  protected SSLSocketFactory socketFactory;

  /** */
  protected SSLServerSocketFactory serverSocketFactory;

  /**
   * Constructs a new secure socket factory generating SSL socket and
   * server socket instances.
   */
  public SecureSocketFactory() {

    Log.log("Initializing secure random (this may take a while) ...");

    new java.security.SecureRandom();

    serverSocketFactory = (SSLServerSocketFactory) 
      SSLServerSocketFactory.getDefault();

    socketFactory = (SSLSocketFactory) SSLSocketFactory.getDefault();

  }

  /**
   * Creates a socket to the target address and port
   */
  public Socket createSocket(InetAddress address, int port)
    throws IOException {
    return(socketFactory.createSocket(address, port));
  }
  
  /**
   * Creates a socket to the target host and port
   */
  public Socket createSocket(String host, int port)
    throws UnknownHostException, IOException {
    return(socketFactory.createSocket(host, port));
  }

  /**
   * Creates a server socket to the target host and port
   */
  public ServerSocket createServerSocket(int port)
    throws IOException {
    SSLServerSocket socket = (SSLServerSocket)
      serverSocketFactory.createServerSocket(port);
    socket.setNeedClientAuth(true);
    return(socket);
  }

  /**
   * Returns true since this factory creates secure connections.
   */
  public boolean isSecure() {
    return(true);
  }
}
