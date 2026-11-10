#!/usr/bin/env python3
import json
import os
import sys
from collections import defaultdict
from datetime import datetime


def format_timestamp(ts_str):
    """Converts Claude ISO timestamps to a cleaner display format."""
    if not ts_str:
        return "N/A"
    try:
        if "." in ts_str:
            ts_str = ts_str.split(".")[0]
        dt = datetime.fromisoformat(ts_str.replace("Z", "+00:00"))
        return dt.strftime("%Y-%m-%d %H:%M:%S")
    except Exception:
        return ts_str


def extract_content(entry):
    """Robustly extracts text content from various Claude JSON structures."""
    # Look across common payload fields
    message = entry.get("message") or entry.get("payload") or entry or {}
    content_field = message.get("content") or entry.get("content") or ""

    if isinstance(content_field, list):
        text_parts = []
        for block in content_field:
            if isinstance(block, dict) and block.get("type") == "text":
                text_parts.append(block.get("text", ""))
            elif isinstance(block, str):
                text_parts.append(block)
        return " ".join(text_parts)
    
    return str(content_field)


def parse_claude_jsonl(filepath):
    """Parses the JSONL file and returns an index of conversations and their raw turns."""
    if not os.path.exists(filepath):
        print(f"Error: File not found at {filepath}", file=sys.stderr)
        sys.exit(1)

    # session_id -> { 'metadata': {}, 'messages': [ (role, text, time) ] }
    sessions = defaultdict(lambda: {"metadata": {}, "messages": []})

    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
                
                sid = entry.get("sessionId") or entry.get("uuid") or "default_session"
                timestamp = entry.get("createdAt") or entry.get("timestamp") or ""
                
                message_obj = entry.get("message") or entry.get("payload") or entry
                role = message_obj.get("role") or entry.get("role") or "unknown"
                
                content = extract_content(entry)

                # Save raw sequence data
                sessions[sid]["messages"].append({
                    "role": role,
                    "content": content,
                    "timestamp": timestamp
                })

            except json.JSONDecodeError:
                continue

    # Post-process summaries for the list view
    for sid, data in sessions.items():
        # Sort messages by timestamp if available
        try:
            data["messages"].sort(key=lambda x: x["timestamp"])
        except Exception:
            pass

        first_msg = next((m for m in data["messages"] if m["role"] == "user"), None)
        preview = "[No text user prompt found]"
        if first_msg and first_msg["content"].strip():
            preview = first_msg["content"].strip().replace("\n", " ")[:60] + "..."

        start_time = data["messages"][0]["timestamp"] if data["messages"] else ""
        
        data["metadata"] = {
            "start_time": format_timestamp(start_time),
            "preview": preview,
            "count": len(data["messages"])
        }

    return sessions


def print_summary_table(sessions):
    """Outputs a scannable index tracking all sessions."""
    header = f"{'Index':<5} | {'Session ID (Prefix)':<20} | {'Started At':<20} | {'Msgs':<5} | {'Initial Prompt Preview'}"
    print("\n" + "=" * 90)
    print(header)
    print("=" * 90)

    for i, (sid, data) in enumerate(sessions.items(), 1):
        meta = data["metadata"]
        print(f"{i:<5} | {sid[:20]:<20} | {meta['start_time']:<20} | {meta['count']:<5} | {meta['preview']}")
    print("=" * 90 + "\n")


def display_transcript(session_id, session_data):
    """Prints a beautiful formatted transcript of a single session."""
    meta = session_data["metadata"]
    print("\n" + "#" * 80)
    print(f"TRANSCRIPT FOR SESSION: {session_id}")
    print(f"Started: {meta['start_time']} | Total Messages: {meta['count']}")
    print("#" * 80 + "\n")

    for msg in session_data["messages"]:
        role = msg["role"].upper()
        time_str = format_timestamp(msg["timestamp"])
        
        # Color coding for terminal clarity
        if role == "USER":
            prefix = f"\033[94m[{time_str}] USER:\033[0m"
        elif role in ("ASSISTANT", "CLAUDE"):
            prefix = f"\033[92m[{time_str}] CLAUDE:\033[0m"
        else:
            prefix = f"\033[93m[{time_str}] {role}:\033[0m"

        print(prefix)
        print(msg["content"].strip())
        print("-" * 60)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 view_claude.py <path_to_claude_file.jsonl> [session_index_or_id]")
        sys.exit(1)

    target_file = sys.argv[1]
    sessions = parse_claude_jsonl(target_file)
    session_keys = list(sessions.keys())

    # If an ID/Index argument was supplied directly via CLI
    if len(sys.argv) == 3:
        target = sys.argv[2]
        if target.isdigit() and 1 <= int(target) <= len(session_keys):
            selected_key = session_keys[int(target) - 1]
        else:
            selected_key = next((k for k in session_keys if k.startswith(target)), None)
        
        if selected_key:
            display_transcript(selected_key, sessions[selected_key])
        else:
            print(f"Error: Could not match session identifier '{target}'")
        sys.exit(0)

    # Otherwise, drop into an interactive viewing mode
    while True:
        print_summary_table(sessions)
        try:
            choice = input("Enter an Index number or Session ID to view (or 'q' to quit): ").strip()
            if choice.lower() in ('q', 'exit'):
                break
            
            if choice.isdigit() and 1 <= int(choice) <= len(session_keys):
                selected_key = session_keys[int(choice) - 1]
            else:
                selected_key = next((k for k in session_keys if k.startswith(choice)), None)

            if selected_key:
                display_transcript(selected_key, sessions[selected_key])
                input("\nPress Enter to return to the session list...")
            else:
                print("\n[!] Invalid selection. Please choose a valid index or ID.")
        except (KeyboardInterrupt, EOFError):
            print("\nExiting.")
            break

