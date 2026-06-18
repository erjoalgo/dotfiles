#!/usr/bin/env python3
"""Fill, rename or list a PDF's fields."""


import argparse

import pypdf

from pypdf import PdfReader, PdfWriter
from pypdf.generic import NameObject, create_string_object

import fillpdf as libfillpdf
from fillpdf import fillpdfs

import fitz

def list_fields(input_pdf):
    """List the fields in input_pdf."""
    reader = PdfReader(input_pdf)
    return (reader.get_fields() or {}).keys()

def print_fields_old(input_pdf):
    # fields = list_fields(input_pdf)
    reader = PdfReader(input_pdf)
    fields = (reader.get_fields() or {})
    print(f"Listing {len(fields)} fields in {input_pdf}...")
    for (name, field) in fields.items():
        print(f"Field Info for {name}: {field}")

def rename_fields(input_pdf, output_pdf, renames):
    """Rename the fields in input_pdf."""
    with open(input_pdf, "rb") as fh:
        writer = PdfWriter(fileobj=fh)

    for page in writer.pages:
        for annot in (page.get("/Annots") or []):
            full_annot = annot.get_object()
            if "/T" not in full_annot:
                continue
            current_name = full_annot["/T"]
            if "checkbox" in current_name:
                continue
            new_name = renames.get(current_name)
            if new_name:
                full_annot[NameObject("/T")] = create_string_object(new_name)

    for field in writer.root_object['/AcroForm']["/Fields"]:
        full_field = field.get_object()
        curr_name = full_field.get("/T")
        if "checkbox" in curr_name:
            continue
        new_name = renames.get(curr_name)
        if new_name:
            full_field[NameObject("/T")] = create_string_object(new_name)

    with open(output_pdf, "wb") as f:
        writer.write(f)

def fill_pdf_pypdf(input_pdf, output_pdf, data_dict):
    """Fill in the fields in input_pdf."""
    with open(input_pdf, "rb") as fh:
        writer = PdfWriter(fileobj = fh)

    print("DDEBUG pdffill.py 99vx: value of data_dict: {}".format(data_dict))
    for i, page in enumerate(writer.pages):
        # non_checkboxes = {k: v for k, v in data_dict.items() if "checkbox" not  in k}
        non_checkboxes = data_dict
        print(f"on page {i+1}")
        writer.update_page_form_field_values(
            page, non_checkboxes,
            auto_regenerate=False)


    """
    fields = writer.get_fields()
    yes_val = "/Yes"
    for (name, field) in fields.items():
        if "checkbox" in name:
            print("DDEBUG pdffill.py iq7e: value of (name, field): {}".format((name, field)))
            # Ensures the checkmark is rendered
            field[NameObject('/V')] = NameObject(yes_val)
            field[NameObject('/AS')] = NameObject(yes_val)
            field[NameObject('/DV')] = NameObject(yes_val)

    # Tell PDF readers to generate appearances
    writer.add_js("this.getField('Check Box 1').value = 'Yes';")

    for page in writer.pages:
        for annot in (page.get('/Annots') or []):
            writer_annot = annot.get_object()
            for field in fields:
                if writer_annot.get('/T') == field:
                    print("DDEBUG fillpdf.py ypdn: value of field: {}".format(field))
                    writer_annot.update({
                        NameObject("/V"): NameObject(fields[field]),
                        NameObject("/AS"): NameObject(fields[field])
                    })
    """

    with open(output_pdf, "wb") as output_stream:
        writer.write(output_stream)


def print_fields(input_pdf):
    field_names = fillpdfs.get_form_fields(input_pdf)
    for (name, value) in field_names.items():
        print(f"{name} = {value}")

def fill_pdf_fillpdfs(input_pdf, output_pdf, data):
    print_fields(input_pdf)
    print("DDEBUG pdffill.py 6fgw: value of data: {}".format(data))
    fillpdfs.write_fillable_pdf(input_pdf, output_pdf, data)
    print_fields(output_pdf)

def list_to_map(arr):
    """Convert a list of key=value strings into a dictionary."""
    d = {}
    for kv in arr:
        if "=" not in kv:
            print(f"warn: skipping mapping with no = sign: {kv}")
            continue

        k, v = kv.split("=")
        if not v:
            print(f"warn: skipping mapping with empty value: {kv}")
            continue
        d[k] = v
    return d

def delete_fields_old(input_pdf, output_pdf, fields_to_delete):
    with open(input_pdf, "rb") as fh:
        writer = PdfWriter(fileobj=fh)

    fields = writer.root_object['/AcroForm']["/Fields"]

    print("DDEBUG pdffill.py slbj: value of fields_to_deBlete: {}".format(fields_to_delete))

    for i in range(len(fields)-1, -1, -1):
        field = fields[i]
        full_field = field.get_object()
        curr_name = full_field.get("/T")
        print("DDEBUG pdffill.py ylm9: value of curr_name: {}".format(curr_name))
        if curr_name in fields_to_delete:
            print(f"deleting field {curr_name}")
            del fields[i]

    # Get the first page
    for page in writer.pages:
        if "/Annots" in page:
            annots = page["/Annots"]
            # Iterate backwards to avoid index shifting
            for i in range(len(annots) - 1, -1, -1):
                annot = annots[i].get_object()
                # Check if it's a widget and if its name is in our delete list
                # print("DDEBUG pdffill.py b3di: value of annot.get('/T'): {}".format(
                #         annot.get('/T')))
                curr_name = annot.get("/T")
                if (annot.get("/Subtype") == "/Widget"
                    and curr_name in fields_to_delete):
                    print(f"deleting field {curr_name}")
                    del annots[i]

    with open(output_pdf, "wb") as output_file:
        writer.write(output_file)


def delete_fields(input_pdf, output_pdf, fields_to_delete):
    doc = fitz.open(input_pdf)
    for page in doc:
        # Get all interactive form widgets on the page
        for widget in page.widgets():
            if widget.field_name in fields_to_delete:
                # 1. Get the internal object reference number
                xref = widget.xref

                # 2. Delete the visual widget widget from the page
                page.delete_widget(widget)

                # 3. Completely clear the internal object definitions
                doc.update_object(xref, "<<>>")

    # Save the updated PDF
    doc.save(output_pdf, incremental=True,
             encryption=fitz.PDF_ENCRYPT_KEEP)
    doc.close()

def main():
    """Main function."""
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="input PDF")
    parser.add_argument("-o", "--output", help="output file")
    parser.add_argument("-l", "--list", action="store_true",
                        help="""list the PDF fields""")
    parser.add_argument("-r", "--rename",
                        help="""rename the given fields.
example: old_name1=new_name1 old_name2=new_name_2""",
                        nargs="+")
    parser.add_argument("-f", "--fill",
                        help="""fill in the given fields.
example: old_name1=new_name1 old_name2=new_name_2""",
                        nargs="+")
    parser.add_argument("-F", "--fill_all",
                        help="""fill in all text fields with their own field names.""",
                        action="store_true")
    parser.add_argument("-d", "--delete", nargs="+",
                        help="""delete the given fields""")
    args= parser.parse_args()

    if args.list:
        print_fields(args.input)

    if not args.output:
        print("warn: making modifications in-place")
        args.output = args.input

    if args.rename:
        renames = list_to_map(args.rename)
        assert args.output
        rename_fields(args.input, args.output, renames)
        print_fields(args.output)

    fill_pdf_fn = fill_pdf_pypdf
    if args.fill:
        fills = list_to_map(args.fill)
        assert args.output
        fill_pdf_fn(args.input, args.output, fills)

    if args.fill_all:
        fields = list_fields(args.input)
        fills = {name: name for name in fields}
        fill_pdf_fn(args.input, args.output, fills)

    if args.delete:
        delete_fields(args.input, args.output, args.delete)
        print_fields(args.output)

if __name__ == "__main__":
    main()

