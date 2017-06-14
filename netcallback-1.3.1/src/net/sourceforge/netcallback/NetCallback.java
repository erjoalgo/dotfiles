/*
 * Virtual Active Network (VAN) Java API
 * Copyright (C) 2000 Alexander V. Konstantinou <akonstan@acm.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 */

package net.sourceforge.netcallback;

import java.io.*;

/**
 * Command-line de-multiplexing between PrivateServer and PublicServer
 *
 * @author Alexander V. Konstantinou (akonstan@acm.org)
 * @version $Revision: 1.1.1.1 $ ; $Date: 2004/02/16 01:17:52 $
 */
public class NetCallback {
  
  /** Prevent instantiation */
  private NetCallback() {
  }

  public static void usage() {
    System.err.println("Starts the public or the private NetCallback server:");

    System.out.print("Usage: -public ");
    PublicServer.usageRaw();

    System.out.print("\nUsage: -private ");
    PrivateServer.usageRaw();
  }

  public static void main(String[] args) {
    if (args.length == 0) {
      usage();
      System.exit(1);
    }

    String[] args2 = new String[args.length - 1];
    System.arraycopy(args, 1, args2, 0, args.length - 1);
    
    if (args[0].equals("-public")) {
      PublicServer.main(args2);
    } else if (args[0].equals("-private")) {
      PrivateServer.main(args2);
    } else {
      usage();
      System.exit(1);
    }
  }
}
