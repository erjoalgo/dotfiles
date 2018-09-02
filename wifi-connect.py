#!/usr/bin/python

"""a simple standalone script to connect to a WPA, WEP or OPN network"""

import sys
import re
import subprocess
from subprocess import call, check_output
import os
import getpass
import logging
import argparse
import time
# import pexpect

logging.basicConfig()
logger = logging.getLogger("wifi-connect")

def call_nonzero(*args, **kwargs):
    "proxy to subprocess.call, raising error on non-zero status"
    logger.debug("calling: "+" ".join(args[0]))
    recode = subprocess.call(*args, **kwargs)
    if recode != 0:
        raise Exception("non-zero exit status")

def verify_commands(commands):
    "ensure commands exist in the system before asking user for passwords"
    pass

def iface_discover():
    "discover the available wifi interfaces"
    ifconfig_out = check_output(["ifconfig", "-a"])
    m = re.findall("(?m)^wlan[0-9]+|wlp[^:]+", ifconfig_out)
    return list(set(m))

def pkill_procs(*procs):
    for proc_name in procs:
        call(["sudo", "pkill", proc_name])

def scan_essids(iface,
                scan_attempts=7,
                scan_results_filename="/tmp/wifi-scan-results"):
    "scan for available access points or cells"
    for _ in range(scan_attempts):
        try:
            cmd=["sudo", "iwlist", iface, "scan"]
            logger.debug("scanning: {}".format(" ".join(cmd)))
            if False:
                iwlist_out = check_output(cmd)
            else:
                iwlist_out = open(scan_results_filename).read()
        except:
            logger.error(iwlist_out)
            logger.info("retrying...")
            time.sleep(1)
            continue

        with open(scan_results_filename, "w") as fh:
            fh.write(iwlist_out)

        cells_flat = re.findall("(?m)^\\s+(Cell [0-9]+.*?\n(^                    .*\n)*)",
                                iwlist_out)
        if not cells_flat:
            print (iwlist_out)
            logger.info("no networks found. retrying...")
            time.sleep(1)
            # import pdb;pdb.set_trace()
            continue
        cells = []
        for cell_flat_groups in cells_flat:
            cell_flat = cell_flat_groups[0]
            cell = {}
            for (attr, regexp) in (("address", "address: (.+)"),
                                   ("channel", "channel:(.*)"),
                                   ("freq", "frequency:(.*)"),
                                   ("quality", "quality=(.*)"),
                                   ("signal", "signal level=:(.*)"),
                                   ("essid", 'essid:"(.*)"'),
                                   ("enc", "encryption key:(.*)"),
                                   ("wpa", "WPA (Version .*)"),
                                   ("wpa2", "WPA2 (Version .*)")):
                m = re.search("(?i){}".format(regexp), cell_flat)
                if m:
                    cell[attr] = m.group(1)
            cells.append(cell)

        return cells

    raise Exception("scanning failed")

def selcand(cands, display_fun=lambda a: str(a), error="no choices available"):
    "interactively select a candidate from a list"
    if not cands:
        raise Exception(error)
    elif len(cands) == 1:
        return cands[0]
    else:
        print ("\n".join("{}: {}".format(i, display_fun(cand)) for (i, cand) in enumerate(cands)))
        idx_string = raw_input("enter selection index: ")
        try:
            return cands[int(idx_string)]
        except:
            import traceback
            traceback.print_exc()
            return selcand(cands, display_fun)

def read_known_essids(directory):
    "read the list of known essids or access points"
    return os.listdir(directory)

def wifi_connect(iface=None, macchange_opt=None, essid=None,
                 encryption=None, overwrite=None, ask_essid=None,
                 password=None, essids_dir=None):
    "connect to one of the available wifi networks"
    if iface is None:
        iface = selcand(iface_discover(), error="wireless iface not found")

    if macchange_opt != None:
        call_nonzero(["sudo", "ifconfig", iface, "down"])
        call_nonzero(["sudo", "macchanger", macchange_opt, iface])

    call_nonzero(["sudo", "ifconfig", iface, "up"])

    if encryption != None and essid != None:
        cell = {"essid":essid, "encryption":encryption}
    else:
        cells = scan_essids(iface)
        assert(cells)

    known_essids = read_known_essids(essids_dir)

    # TODO named tuple
    common = set(cell["essid"] for cell in cells).intersection(known_essids)
    if common and not ask_essid:
        essid = selcand(list(common))
        cell=[cell for cell in cells if cell["essid"] == essid][0]
    else:
        # TODO visually paint quality
        cell = selcand(cells, lambda cell: "{} ({})".format(cell["essid"], cell["quality"]))

    essid_path = os.path.join(essids_dir, cell["essid"])

    if cell["enc"] == "on":
        if overwrite or not os.path.exists(essid_path):
            if not password:
                password = getpass.getpass("enter password for {}: ".format(cell["essid"]))
        else:
            with open(essid_path, "r") as fh:
                password = fh.read()

    pkill_procs("wpa_supplicant", "dhclient")
    if "wpa" in cell or "wpa2" in cell:
        if password != None:
            with open(essid_path, "w") as fh:
                # TODO echo err message on failure
                call_nonzero(["wpa_passphrase", cell["essid"], password], stdout=fh)
        # TODO use a list
        # p = pexpect.spawn("spawn sudo wpa_supplicant -i {} -c {} -D nl80211,wext"
        #                   .format(iface, cell["essid"]))
        # p.logfile_read = sys.stderr
        # p.expect(["CTRL-EVENT-CONNECTED", ])
        print (essid_path)
        p=subprocess.Popen(["sudo", "wpa_supplicant",
                            "-i", iface, "-c", essid_path, "-D", "nl80211,wext"],
                           stdout=subprocess.PIPE)
        for line in iter(p.stdout.readline, ""):
            print (line)
            if "CTRL-EVENT-CONNECTED" in line:
                break
        else:
            raise Exception("failed to connect via wpa_supplicant")

        p.stdout.close()
    else:
        # wep or open
        cmd = ["sudo", "iwconfig", iface, "essid", cell["essid"]]
        if cell["enc"] == "on":
            cmd += ["enc", "WEP", "key", password]
        call_nonzero(cmd)

    call_nonzero(["sudo", "dhclient", "-v", iface])


def main():
    "main method"
    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--overwrite", action="store_true",
                        help="flag to disregard any existing password")
    parser.add_argument("-e", "--essid",
                        help="connect to given essid. required when connecting to a hidden essid")
    parser.add_argument("-p", "--password",
                        help="the password, option to avoid interactive prompt")
    parser.add_argument("-m", "--macchange-opt",
                        help="a one-character flag proxied to macchanger")
    parser.add_argument("-i", "--iface",
                        help="the wireless interface to avoid discovery")
    parser.add_argument("-a", "--ask-essid", action="store_true",
                        help="always prompt for essid, even if matching entry exists")
    parser.add_argument("-E", "--encryption",
                        help="sets the network's encryption. "+
                        "required when connecting to a hidden essid")
    parser.add_argument("-v", "--verbose", action="store_true")

    args = parser.parse_args()
    if args.verbose:
        logger.setLevel(logging.DEBUG)
        logger.debug("verbose on...")

    del args.verbose

    args.essids_dir=os.path.expanduser("~/.config/wifi-connect")
    wifi_connect(**vars(args))

if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "./wifi-connect.py -v"
# End:
