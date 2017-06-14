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

import java.util.Map;
import java.util.HashMap;

/**
 * Server executing on the host behind the firewall that connects to
 * the protected host as well as the public host (running a PublicServer)
 * and then copies traffic between the two.
 * 
 * @see PublicServer
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:54 $
 */
public class PrivateServer extends Thread implements SocketBridgeListener {

  /** Socket to PublicServer */
  protected Socket socket;

  /** Data input stream from PublicServer */
  protected DataInputStream istream;

  /** Data output stream from PublicServer */
  protected DataOutputStream ostream;
  
  /** True if the thread should be active, false otherwise */
  protected boolean active = true;

  /** Name of the host running the PublicServer */
  protected String publicHost;

  /** Port of the PublicServer */
  protected int publicPort;

  /** Port number used by the PublicServer to accept TCP redirections */
  protected int publicTcpPort;

  /** Port number used by the PublicServer to accept UDP redirections */
  protected int publicUdpPort;
  
  /** Host ame of the redirection target */
  protected String tcpHost;

  /** Port of the redirection target */
  protected int privatePort;

  /** Maps socket bridge objects to public ID numbers (Long) */
  protected Map socketBridge2IdMap = new HashMap();

  /** Maps outgoing port to socket bridge objects */
  protected Map outPort2SocketBridgeMap = new HashMap();

  /** The socket factory used to create Socket and ServerSocket instances */
  protected CallbackSocketFactory socketFactory;

  /** Maps the UDP datagram source_address:source_port to the local 
   *  DatagramSocket redirect */
  protected Map srcDatagramSocketMap;

  /** Address of private UDP server (may be null)*/
  protected InetAddress udpHost;
  
  /** Port of private UDP server*/
  protected int udpPort;

  /**
   * Construct a private server receiving requests from the public host
   * and forwarding them to the private host.
   *
   * @param publicHost - the host where the PublicServer is running
   * @param publicPort - the port where the PublicServer is bound-to
   * @param tcpHost - the internal host that is the target of the
   *                      redirection.
   * @param privatePort - the port in the internal host that is the target
   *                      of the redirection.
   * @param socketFactory - used to create Socket and ServerSocket instances
   *
   * @exception IOException - if the PublicServer running on 
   *                          publicHost:publicPort could not be contacted,
   *                          a communication error with the PublicServer
   *                          occured, or the remote host is unknown.
   * 
   * @see PublicServer
   */
  public PrivateServer(String publicHost, int publicPort,
		       String tcpHost, int privatePort,
		       String udpHost, int udpPort,
		       CallbackSocketFactory socketFactory)
    throws IOException {

    this.publicHost = publicHost;
    this.publicPort = publicPort;
    this.tcpHost = tcpHost;
    this.privatePort = privatePort;
    if (udpHost != null) {
      this.udpHost = InetAddress.getByName(udpHost);
    }
    this.udpPort = udpPort;
    this.socketFactory = socketFactory;
    this.srcDatagramSocketMap = new HashMap();

    // Verify private host is resolvable (at least right now)
    InetAddress.getByName(tcpHost);

    // Connect to PublicServer
    socket = socketFactory.createSocket(publicHost, publicPort);
    istream = new DataInputStream(socket.getInputStream());
    ostream = new DataOutputStream
      (new BufferedOutputStream(socket.getOutputStream()));
    
    //
    // Send: CONNECT VERSION
    //
    synchronized(ostream) {
      ostream.writeInt(CallbackProtocol.CONNECT);
      ostream.writeInt(CallbackProtocol.VERSION);
      ostream.flush();
    }
    
    // 
    // Read: CONNECT VERSION REDIRECT_PORT
    //
    int operation = istream.readInt();

    if (operation != CallbackProtocol.CONNECT) {
      throw new IOException("PublicServer responded with unknown operation " +
			    CallbackProtocol.getOperationDescription
			    (operation));
    }

    int version = istream.readInt();

    if (version != CallbackProtocol.VERSION) {
      throw new IOException("PublicServer responded with unsupported " +
			    " version " + version);
    }

    publicTcpPort = istream.readInt();
    publicUdpPort = istream.readInt();

    if (tcpHost != null) {
      Log.log("Exposing TCP " + tcpHost + ":" + privatePort +
	      " to programs that can access TCP " + publicHost + ":" +
	      publicTcpPort);
    }

    if (udpHost != null) {
      Log.log("Exposing UDP " + udpHost + ":" + udpPort +
	      " to programs that can access UDP " + publicHost + ":" +
	      publicUdpPort);
    }

    start();

    //
    // Ping thread (used to ping server)
    //
    PingThread pingThread = new PingThread(ostream);
    pingThread.start();
  }

  /**
   * Returns the DatagramSocket which should be used to communicate
   * with the protected UDP server so that the originating source
   * can be identified.
   * <p>
   * The DatagramSocket is created and then reused for subsequent requests.
   */
  protected synchronized DatagramSocket getDatagramSocket
      (final InetAddress srcAddress, final int srcPort)
    throws SocketException {

    String key = srcAddress.getHostAddress() + ":" + srcPort;

    DatagramSocket socket = (DatagramSocket) srcDatagramSocketMap.get(key);

    if (socket == null) {
      socket = new DatagramSocket();
      srcDatagramSocketMap.put(key, socket);

      final DatagramSocket finalSocket = socket;
      
      Thread thread = new Thread() {
	public void run() {
	  byte[] buffer = new byte[65536];
	  DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
	  
	  while(true) {
	    try {
	      packet.setData(buffer);
	      packet.setLength(buffer.length);
	      finalSocket.receive(packet);
	      
	      Log.debug("Received datagram packet from " +
			packet.getAddress() + ":" + packet.getPort());
	      try {

		synchronized(ostream) {
		  ostream.writeInt(CallbackProtocol.DATAGRAM);
		  ostream.write(srcAddress.getAddress());
		  ostream.writeInt(srcPort);
		  ostream.writeInt(packet.getLength());
		  ostream.write(packet.getData(), 0, packet.getLength());
		  ostream.flush();
		} 

	      } catch (Throwable e) {
		Log.log("Error writting UDP datagram to tunnel: " +
			e.getClass().getName() + ": " + e.getMessage());
	      }
	    } catch (Throwable e) {
	      Log.log("Error receiving datagram: " + e.getClass().getName() +
		      ": " + e.getMessage());
	    }
	  }
	}
      };
      thread.setDaemon(true);
      thread.start();
    }
    
    return(socket);
  }

  /**
   * While active, loop receiving requests on the established socket from
   * the public server.  For each request, connect to the temporary
   * socket on the PublicServer as well as to the private host:port, and
   * then copy traffic between the two.
   */
  public void run() {
    try {
      while(active) {
	// REQUEST: PORT

	int operation = istream.readInt();
	
	Log.debug("REQUEST " + 
		  CallbackProtocol.getOperationDescription(operation));
		  
	if (operation == CallbackProtocol.PING) {
	  // no-op
	} else if (operation == CallbackProtocol.CLOSE) {
	  int port = istream.readInt();
	  
	  SocketBridge socketBridge = (SocketBridge)
	    outPort2SocketBridgeMap.remove(new Integer(port));

	  if (socketBridge != null) {
	    socketBridge.terminate();
	  }
	} else if (operation == CallbackProtocol.DATAGRAM) {
	  //
	  // SRC_HOST SRC_PORT LENGTH DATA
	  //

	  byte[] srcAddressBytes = new byte[4];
	  istream.readFully(srcAddressBytes);
	  int srcPort = istream.readInt();

	  int length = istream.readInt();
	  byte[] data = new byte[length];
	  istream.readFully(data);

	  InetAddress srcAddress = 
	    InetAddressUtils.getByAddress(srcAddressBytes);

	  Log.debug("Received UDP datagram over tunnel from " +
		    srcAddress + ":" + srcPort);

	  if (udpHost == null) {
	    Log.log("Ignoring UDP forwarding request from " + 
		    srcAddress + ":" + srcPort + ": UDP forwarding has " +
		    "not been configured on PrivateServer");
	    continue;
	  }

	  try {
	    DatagramSocket udpSocket = getDatagramSocket(srcAddress, srcPort);
	    
	    DatagramPacket packet = new DatagramPacket
	      (data, length, udpHost, udpPort);
	    Log.debug("Forwarding datagram to " + udpHost + ":" + udpPort);
	    
	    udpSocket.send(packet);
	  } catch (Throwable e) {
	    Log.log("Error sending UDP datagram to service:" +
		    e.getClass().getName() + ": "+ e.getMessage());
	  }

	  
	} else if (operation == CallbackProtocol.CALLBACK) {
	  int tempPort = istream.readInt();
	  long id = istream.readLong();

	  if (socketFactory.isSecure()) {
	    Log.log("Securely bridging " + tcpHost + ":" + privatePort + 
		    " -> " + publicHost + ":" + tempPort);
	  } else {
	    Log.log("Cleartext bridging " + tcpHost + ":" + privatePort + 
		    " -> " + publicHost + ":" + tempPort);
	  }
	  
	  Socket publicSocket = null;
	  Socket privateSocket = null;
	  
	  try {
	    privateSocket = new Socket(tcpHost, privatePort);
	    privateSocket.setSoTimeout(1000);

	  } catch (IOException e) {
	    Log.log("Error connecting to private host " + 
		    tcpHost + ":" + privatePort +
		    " (verify that target service is running): " +
		    e.getMessage());
	    continue;
	  }
	  
	  try {
	    publicSocket = socketFactory.createSocket(publicHost, tempPort);
	    if (socketFactory instanceof DefaultSocketFactory) {
	      publicSocket.setSoTimeout(1000);
	    }

	  } catch(Throwable e) {
	    try {
	      Log.debug("Closing private socket " + privateSocket);
	      privateSocket.close();
	    } catch (Exception e2) { }
	    
	    Log.log("Error connecting to public host " + 
		    publicHost + 
		    " on temporary port " + tempPort +
		    " (make sure public host is not protected by" +
		    " a firewall): " + e.getMessage());
	    continue;
	  }
	  						  
	  try {
	    SocketBridge socketBridge = 
	      new SocketBridge(publicSocket, privateSocket, this);

	    socketBridge2IdMap.put(socketBridge, new Long(id));

	    outPort2SocketBridgeMap.put
	      (new Integer(publicSocket.getLocalPort()), socketBridge);

	  } catch (IOException e) {
	    Log.log("Error establishing bridge between hosts: " +
			       e.getMessage());
	    try {
	      Log.debug("Closing private socket " + privateSocket);
	      privateSocket.close();
	      Log.debug("Closing public socket " + publicSocket);
	      publicSocket.close();
	    } catch (Exception e2) {
	    }
	  }
	} else {
	  // unknown operation
	  Log.log("Unknown public server operation " + 
		  CallbackProtocol.getOperationDescription
		  (operation));
	}
      } // while
    } catch (IOException e) {
      Log.log("PrivateServer exiting due to communication error with PublicServer: " + e.getMessage());
    } catch (Throwable e) {
      Log.log("PrivateServer exiting due to unexpected error: " + 
	      e.getClass().getName() + ": " + e.getMessage());
    }
  }

  // SocketBridgeListener /////////////////////////////////////////////////////

  /**
   * Invoked when a socket bridge has closed (terminated)
   */
  public void socketBridgeClosed(SocketBridge s) {
    Long id = (Long) socketBridge2IdMap.remove(s);

    if (id != null) {
      try {
	synchronized(ostream) {
	  ostream.writeInt(CallbackProtocol.CLOSE);
	  ostream.writeLong(id.longValue());
	  ostream.flush();
	} 
      } catch (Throwable e) {
	e.printStackTrace();
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////

  /**
   * Prints out usaga and exits
   */
  protected static void usageRaw() {
    System.err.println("{ -ssl } -service <host> <port>" +
		       " { -tcp <host> <port> }" +
		       " { -udp <host> <port> }");
  }

  /**
   * Prints out usagae and exits
   */
  protected static void usageExit() {
    System.out.print("Usage: ");
    usageRaw();
    System.exit(1);
  }

  /**
   * Command-line startup of PrivateServer
   */
  public static void main(String[] args) {

    String pname = "PrivateServer";

    CallbackSocketFactory socketFactory = null;

    String serviceHost = null;
    int servicePort = -1;

    String tcpHost = null;
    int tcpPort = -1;
    
    String udpHost = null;
    int udpPort = 0;

    //
    // Parse commandline arguments
    //
    for(int i=0; i<args.length; i++) {
      
      if (args[i].equals("-service")) {
	if (serviceHost != null) usageExit();
	serviceHost = CommandLine.getStringArg(pname, args[i], args, ++i);
	servicePort = CommandLine.getIntArg(pname, args[i], args, ++i);

      } else if (args[i].equals("-tcp")) {
	if (tcpHost != null) usageExit();
	tcpHost = CommandLine.getStringArg(pname, args[i], args, ++i);
	tcpPort = CommandLine.getIntArg(pname, args[i], args, ++i);

      } else if (args[i].equals("-udp")) {
	if (udpHost != null) usageExit();
	udpHost = CommandLine.getStringArg(pname, args[i], args, ++i);
	udpPort = CommandLine.getIntArg(pname, args[i], args, ++i);
	
      } else if (args[i].equals("-ssl")) {
	//
	// Verify SSL Environment
	//
	
	try {
	  Class.forName("javax.net.ssl.SSLSocketFactory");
	} catch (Throwable e) {
	  System.err.println(pname + ": Missing Java JSSE libraries (make sure .jar files are installed in $JAVA_HOME/jre/lib/ext)");
	  System.exit(1);
	}
	
	String truststore = System.getProperty("javax.net.ssl.trustStore");
	if (truststore == null) {
	  System.err.println(pname + ": property javax.net.ssl.trustStore is undefined");
	  System.exit(1);
	}
	
	File truststoreFile = new File(truststore);
	if (!truststoreFile.exists()) {
	  System.err.println(pname + ": truststore file '" + truststore + 
			     "' not found");
	  System.exit(1);
	}
	
	socketFactory = new SecureSocketFactory();
      } else {
	System.err.println(pname + ": unknown argument " + args[i]);
	usageExit();
      } 
    } // for each argument

    //
    // Validate flags
    //
    if (socketFactory == null) {
      socketFactory = new DefaultSocketFactory();
    }

    if (serviceHost == null) {
      System.err.println(pname + ": must provide -service argument");
      usageExit();
    }
    
    if ( (tcpHost == null) && (udpHost == null) ) {
      System.err.println(pname + ": did not provide either a -tcp or a -udp flag");
      usageExit();
    }
    
    //
    // Create PrivateServer instance
    //	
    try {
      PrivateServer daemon = new PrivateServer(serviceHost, servicePort,
					       tcpHost, tcpPort,
					       udpHost, udpPort,
					       socketFactory);
      daemon.join();
    } catch (java.net.ConnectException e) {
      System.err.println(pname + ": Error connecting to " + serviceHost + ":" +
			 servicePort);
      System.err.println("     (verify that the public server has been started)");
      System.exit(1);
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
    System.exit(0);
  }
}

