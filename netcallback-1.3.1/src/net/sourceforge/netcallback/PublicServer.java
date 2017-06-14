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
 * Server executing on a public host (not protected by a firewall) which
 * is the starting point of the redirection.
 * <p>
 * The public server listens on two ports:
 * <ul>
 * <li>one port for the connection with the PrivateServer (using the 
 *     netcallback protocol)
 * <li>another port for port redirection requests (in essense, this port
 *     appears to processes as the redirection target port).
 * </uL>
 * 
 * @see PrivateServer
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:55 $
 */
public class PublicServer extends Thread {
  
  /** Server socket used to receive the PrivateServer connection */
  protected ServerSocket serviceServerSocket;

  /** Server socket used to receive redirection requests */
  protected ServerSocket tcpServerSocket;

  /** UDP Datagram socket used to send and receive datagrams */
  protected final DatagramSocket udpSocket;

  /** UDP receiver thread */
  protected final Thread udpThread;

  /** True while the thread should be active */
  protected boolean active = true;

  /** Socket to PrivateServer */
  protected Socket requestSocket = null;

  /** Data output stream to Private Server (used to send callback requests) */
  protected DataOutputStream serviceDataOutput;

  /** Data input stream from Private Server (used for ping/close receives) */
  protected DataInputStream serviceDataInput;

  /** Thread used to wait for the PrivateServer connection */
  protected Thread serviceThread;

  /** Maps connection id values (Long) to SocketBridge objects */
  protected Map id2SocketBridgeMap = 
    java.util.Collections.synchronizedMap(new HashMap());

  /** The next connection ID to be assigned (synchronize on nextIDLock) */
  protected long nextID = 1;

  /** Controlls access to nextID */
  protected Object nextIDLock = new Object();

  /** The socket factory used to create Socket and ServerSocket instances */
  protected CallbackSocketFactory socketFactory;

  /**
   * Construct a new PublicServer waiting for connection from a
   * PrivateServer on servicePort, and exporting redirects in the
   * tcpPort.
   * 
   * @param servicePort - port on which PrivateServer will contact this object
   * @param tcpPort - port to which clients can connect to receive
   *                       redirection service.
   * @param socketFactory - used to create Socket and ServerSocket instances
   *
   * @exception IOException - if one of the server sockets could not be
   *                          created.
   */
  public PublicServer(final int servicePort, 
		      final int tcpPort,
		      final int udpPort,
		      CallbackSocketFactory socketFactory)
    throws IOException {
    
    this.socketFactory = socketFactory;

    serviceServerSocket = socketFactory.createServerSocket(servicePort);

    //
    // TCP server
    //
    if (tcpPort > 0) {
      try {
	tcpServerSocket = new ServerSocket(tcpPort);
      } catch (IOException e) {
	try { serviceServerSocket.close(); } catch (Throwable e2) { }
	throw e;
      }

      Log.log("Redirecting requests from TCP port " + tcpPort + 
	      " to the remote host connected to TCP port " + servicePort);
    } else {
      tcpServerSocket = null;
    }

    //
    // UDP server
    //
    if (udpPort > 0) {
      udpSocket = new DatagramSocket(udpPort);
      
      udpThread = new Thread() {
	public void run() {
	  byte[] buffer = new byte[65536];
	  DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
	  
	  while(true) {
	    try {
	      packet.setData(buffer);
	      packet.setLength(buffer.length);
	      udpSocket.receive(packet);
	      
	      DataOutputStream ostream = getServiceDataOutput();
	      Log.debug("Received UDP datagram from " + 
			packet.getAddress() + ":" + packet.getPort());

	      if (ostream == null) {
		Log.log("Discarding UDP packet from " + packet.getAddress() + 
			":" + packet.getPort() + ": tunnel not set up");
	      } else {
		//
		// SRC_HOST SRC_PORT LENGTH DATA
		//
		try {
		  synchronized(ostream) {
		    ostream.writeInt(CallbackProtocol.DATAGRAM);
		    ostream.write(packet.getAddress().getAddress());
		    ostream.writeInt(packet.getPort());
		    ostream.writeInt(packet.getLength());
		    ostream.write(packet.getData(), 0, packet.getLength());
		    ostream.flush();
		  }
		} catch (Throwable e) {
		  Log.log("Error writting UDP datagram to tunnel: " +
			  e.getClass().getName() + ": " + e.getMessage());
		}
	      }
	    } catch (Throwable e) {
	      Log.log("Error receiving datagram: " + e.getClass().getName() +
		      ": " + e.getMessage());
	    }
	  }
	}
      };

      udpThread.setDaemon(true);
      udpThread.start();

      Log.log("Tunnelling UDP datagrams received on UDP port " + udpPort + 
	      " to the remote host connected to TCP port " + servicePort);
    } else {
      // udpPort <= 0
      udpSocket = null;
      udpThread = null;
    }

    //
    // Start the thread accepting a connection from the PrivateServer
    // (NOTE: only one PrivateServer at a time !)
    //

    Thread serviceThread = new Thread() {
      public void run() {
	while(active) {
	  try {
	    Socket socket = serviceServerSocket.accept();
	    Log.log("PrivateServer connect from " + 
		    socket.getInetAddress() + ":" +
		    socket.getPort());
	    requestSocket = socket;
	    
	    // Use temporary variable to prevent race condition
	    // (first value must be public redirect port)
	    final DataOutputStream ostream = new DataOutputStream
	      (new BufferedOutputStream(socket.getOutputStream()));
	    final DataInputStream istream =
	      new DataInputStream(socket.getInputStream());

	    //
	    // Read: CONNECT <version>
	    //

	    int peerOperation = istream.readInt();
	    if (peerOperation != CallbackProtocol.CONNECT) {
	      Log.log("Invalid server operation " +
		      CallbackProtocol.getOperationDescription
		      (peerOperation));
	      try {
		ostream.close();
		istream.close();
		socket.close();
	      } catch (Throwable e) { }
	      continue;
	    }
				 
	    int peerVersion = istream.readInt();

	    // 
	    // Write: CONNECT VERSION REDIRECT_PORT
	    //

	    ostream.writeInt(CallbackProtocol.CONNECT);
	    ostream.writeInt(CallbackProtocol.VERSION);
	    ostream.writeInt(tcpPort);
	    ostream.writeInt(udpPort);
	    ostream.flush();

	    //
	    // Protocol thread (used to receive PING/CLOSE/etc.)
	    //
	    Thread protocolThread = new Thread() {
	      public void run() {
		boolean active = true;
		try {
		  while(active) {
		    int operation = istream.readInt();
		    Log.debug("REQUEST " + 
			      CallbackProtocol.getOperationDescription
			      (operation));
		    
		    switch(operation) {

		    case CallbackProtocol.DATAGRAM:
		      //
		      // DEST_HOST DEST_PORT LENGTH DATA
		      //
		      byte[] destAddressBytes = new byte[4];
		      istream.readFully(destAddressBytes);

		      int destPort = istream.readInt();

		      int length = istream.readInt();

		      byte[] data = new byte[length];
		      istream.readFully(data);

		      InetAddress destAddress = 
			InetAddressUtils.getByAddress(destAddressBytes);

		      Log.debug("Received UDP datagram destined to: " +
				destAddress + ":" + destPort);

		      if (udpSocket == null) {
			Log.log("Discarding UDP datagram destined to: " +
				destAddress + ":" + destPort + ": " +
				"UDP socket has not been configured!");
			
		      } else {
			try {
			  DatagramPacket packet = new DatagramPacket
			    (data, length, destAddress, destPort);
			  
			  udpSocket.send(packet);
			} catch (Throwable e) {
			  Log.log("Error sending UDP datagram to client:" +
				  e.getClass().getName() + ": " +
				  e.getMessage());
			}
		      }
		      break;

		    case CallbackProtocol.PING: 
		      // no-op
		      break;

		    case CallbackProtocol.CLOSE:
		      long id = istream.readLong();
		      
		      SocketBridge socketBridge =
			(SocketBridge) id2SocketBridgeMap.remove(new Long(id));
		      
		      if (socketBridge != null) {
			socketBridge.terminate();
		      }
		      break;
		    default:
		      Log.log("Received unexpected operation from PrivateServer " + CallbackProtocol.getOperationDescription(operation));
		    }
		  }
		} catch (EOFException e) {
		  Log.log("Connection to PrivateServer lost");
		} catch (Throwable e) {
		  e.printStackTrace();
		}
	      }
	    };
	    protocolThread.start();

	    //
	    // Ping thread (used to ping server)
	    //
	    PingThread pingThread = new PingThread(ostream);
	    pingThread.start();

	    //
	    //
	    // Connection established
	    //

	    serviceDataOutput = ostream;
	    serviceDataInput = istream;
	  } catch (Throwable e) {

	    if (e.getClass().getName().equals
		("javax.net.ssl.SSLException")) {
	      Log.log("SSLException: " + e.getMessage());
	    } else {
	      e.printStackTrace();
	    }
	  }
	}
      }
    };
    serviceThread.start();
    
    //
    // Start the main thread which accepts redirection requests (by connecting
    // to the redirect port) and forwards them to the Private Server
    //    
    if (tcpServerSocket != null) {
      start();
    }
  }

  /**
   * Returns the next unique ID for a connection
   */
  public long getNextID() {
    long result;

    synchronized(nextIDLock) {
      result = nextID;
      nextID++;
    }

    return(result);
  }

  /**
   * Returns the DataOutputStream used to tunnel
   */
  public DataOutputStream getServiceDataOutput() {
    return(serviceDataOutput);
  }

  /**
   * Main thread accepting connections to the redirect port, binding
   * a new copying thread to an anonymous port, and requesting the
   * private server to connect to the anonymous port.
   */
  public void run() {
    while(active) {
      try {
	// Accept a redirect request
	final Socket clientSocket = tcpServerSocket.accept();
	clientSocket.setSoTimeout(1000);

	if (socketFactory.isSecure()) {
	  Log.log("Secure redirect request from " + 
		  clientSocket.getInetAddress() + ":" +
		  clientSocket.getPort());
	} else {
	  Log.log("Cleartext redirect request from " + 
		  clientSocket.getInetAddress() + ":" +
		  clientSocket.getPort());
	}
	  
	if (serviceDataOutput == null) {
	  Log.log("Rejecting redirect request because private server has not been started");
	  clientSocket.close();
	  continue;
	}
    

	// Find an anonymous port
	ServerSocket server = null;
	int anonPort = 10000;
	while(server == null) {
	  try {
	    server= socketFactory.createServerSocket(anonPort);
	  } catch (IOException e) {
	    anonPort++;
	  }
	}
	
	final ServerSocket finalServer = server;
	final int finalAnonPort = anonPort;
	final long id = getNextID();

	// Create the thread that will wait for the PrivateServer
	// callback and then start the copying process.
	Thread callbackWaitThread = new Thread() {
	  public void run() {
	    this.setName("CallbackWait[" + finalAnonPort + "]");
	    Socket callbackSocket = null;
	    try {
	      callbackSocket = finalServer.accept();

	      if (socketFactory instanceof DefaultSocketFactory) {
		callbackSocket.setSoTimeout(1000);
	      }

	      final int callbackSocketPort = callbackSocket.getPort();
	      
	      // Close anonymous port server
	      try {
		Log.debug("Closing final server socket " + finalServer);
		finalServer.close();
	      } catch (Throwable e) {
		e.printStackTrace();
	      }

	      SocketBridgeListener l = new SocketBridgeListener() {
		public void socketBridgeClosed(SocketBridge s) {
		  id2SocketBridgeMap.remove(new Long(id));

		  synchronized(serviceDataOutput) {
		    try {
		      serviceDataOutput.writeInt(CallbackProtocol.CLOSE);
		      serviceDataOutput.writeInt(callbackSocketPort);
		      serviceDataOutput.flush();
		    } catch (IOException e) {
		      e.printStackTrace();
		    }
		  }
		}
 	      };

	      SocketBridge socketBridge = 
		new SocketBridge(clientSocket, callbackSocket, l);

	      id2SocketBridgeMap.put(new Long(id), socketBridge);
	    } catch (Throwable e) {

	      e.printStackTrace();
	      try {
		Log.debug("Closing callback socket " + callbackSocket);
		callbackSocket.close();
	      } catch (Throwable e2) { }
		  
	    }
	  }
	};
	callbackWaitThread.start();
    
	try {
	  // CALLBACK PORT ID
	  synchronized(serviceDataOutput) {
	    serviceDataOutput.writeInt(CallbackProtocol.CALLBACK);
	    serviceDataOutput.writeInt(anonPort);
	    serviceDataOutput.writeLong(id);
	    serviceDataOutput.flush();
	  }
	} catch (IOException e) {
	  Log.debug("Closing client socket " + clientSocket);
	  clientSocket.close();
	}
      } catch (Throwable e) {
	if (e.getClass().getName().equals
	    ("javax.net.ssl.SSLException")) {
	  Log.log("SSLException: " + e.getMessage());
	} else {
	  e.printStackTrace();
	}
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////

  /**
   * Prints command-line usage
   */
  protected static void usageRaw() {
    System.err.println("{ -ssl } -servicePort <port>" +
		       " { -tcpPort <port> } { -udpPort <port> }");
  }
  
  /**
   * Prints command-line usage and exits(1)
   */
  protected static void usageExit() {
    System.out.print("Usage: ");
    usageRaw();
    System.exit(1);
  }

  /**
   * Command line invocation of the PublicServer
   */
  public static void main(String[] args) {
    
    String pname = "PublicServer";
    
    int servicePort = -1;
    int tcpPort = -1;
    int udpPort = -1;

    CallbackSocketFactory socketFactory = null;

    //
    // Parse commandline arguments
    //
    for(int i=0; i<args.length; i++) {

      if (args[i].equals("-servicePort")) {
	if (servicePort > 0) usageExit();
	servicePort = CommandLine.getIntArg(pname, args[i], args, ++i);

      } else if (args[i].equals("-tcpPort")) {
	if (tcpPort > 0) usageExit();
	tcpPort = CommandLine.getIntArg(pname, args[i], args, ++i);

      } else if (args[i].equals("-udpPort")) {
	if (udpPort > 0) usageExit();
	udpPort = CommandLine.getIntArg(pname, args[i], args, ++i);
	
      } else if (args[i].equals("-ssl")) {
	//
	// Verify SSL environment
	//
	
	try {
	  Class.forName("javax.net.ssl.SSLSocketFactory");
	} catch (Throwable e) {
	  System.err.println(pname + ": Missing Java JSSE libraries (make sure .jar files are installed in $JAVA_HOME/jre/lib/ext): " + e.getMessage() );
	  System.exit(1);
	}
	
	String keystore = System.getProperty("javax.net.ssl.keyStore");
	if (keystore == null) {
	  System.err.println(pname +
			     ": property javax.net.ssl.keyStore is undefined");
	  System.exit(1);
	}
	
	File keystoreFile = new File(keystore);
	if (!keystoreFile.exists()) {
	  System.err.println(pname + ": keystore file '" + keystore + 
			     "' not found");
	  System.exit(1);
	}
	
	String password= System.getProperty("javax.net.ssl.keyStorePassword");
	while( (password == null) || (password.length() == 0) ) {
	  System.out.print("Please enter javax.net.ssl.keyStorePassword: ");
	  BufferedReader reader = new BufferedReader
	    (new InputStreamReader(System.in));
	  try {
	    password = reader.readLine();
	  } catch (IOException e) {
	    System.err.println(pname + ": error reading keyStorePassword: " +
			       e.getMessage());
	    System.exit(1);
	  }
	  
	  try {
	    System.setProperty("javax.net.ssl.keyStorePassword", password);
	  } catch (Throwable e) {
	    System.err.println(pname + ": error setting javax.net.ssl.keyStorePassword: " + e.getMessage());
	    System.exit(1);
	  }
	} // while (password not set)
	
	socketFactory = new SecureSocketFactory();
      } else {
	System.err.println(pname + ": unknown argument " + args[i]);
	usageExit();
      }
    } // for (each argument)

    //
    // Validate flags
    //
    if (socketFactory == null) {
      socketFactory = new DefaultSocketFactory();
    }

    if (servicePort <= 0) {
      System.err.println(pname + ": must provide -servicePort argument");
      usageExit();
    }
    
    if ( (tcpPort <= 0) && (udpPort <= 0) ) {
      System.err.println(pname + ": one of -tcpPort or -udpPort must be provided");
      usageExit();
    }

    //
    // Create PublicServer instance
    //	
    try {
      PublicServer daemon = new PublicServer(servicePort, tcpPort,
					     udpPort, socketFactory);
      synchronized(daemon) {
	daemon.wait();
      }
    } catch (Exception e) {
      Log.log(pname + ": " + e.getClass().getName() + ": " +
	      e.getMessage());
      System.exit(1);
    }
  }
}
