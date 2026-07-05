#!/usr/bin/env python3

"""Fill, rename or list a PDF's fields."""


import argparse
import logging

from fillpdf import fillpdfs
from pypdf import PdfReader, PdfWriter
from pypdf.generic import NameObject, create_string_object
import pymupdf


def list_fields(input_pdf):
    """List the fields in input_pdf."""
    reader = PdfReader(input_pdf)
    return (reader.get_fields() or {}).keys()

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

    for i, page in enumerate(writer.pages):
        # non_checkboxes = {k: v for k, v in data_dict.items() if "checkbox" not  in k}
        non_checkboxes = data_dict
        logging.debug(f"on page %s{i+1}")
        writer.update_page_form_field_values(
            page, non_checkboxes,
            auto_regenerate=False)


    with open(output_pdf, "wb") as output_stream:
        writer.write(output_stream)


def print_fields(input_pdf):
    """Print the field names in the pdf."""
    field_names = fillpdfs.get_form_fields(input_pdf)
    for (name, value) in field_names.items():
        print(f"{name} = {value}")


def list_to_map(arr):
    """Convert a list of key=value strings into a dictionary."""
    d = {}
    for kv in arr:
        if "=" not in kv:
            logging.info("warn: skipping mapping with no = sign: %s", kv)
            continue

        k, v = kv.split("=")
        if not v:
            logging.info("warn: skipping mapping with empty value: %s", kv)
            continue
        d[k] = v
    return d


def delete_fields(input_pdf, output_pdf, fields_to_delete):
    """Delete the given fields and write to output_pdf."""
    doc = pymupdf.open(input_pdf)
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
             encryption=pymupdf.PDF_ENCRYPT_KEEP)
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
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="""verbose logging""")

    args= parser.parse_args()

    logging.basicConfig(level=logging.DEBUG
                        if args.verbose else logging.INFO)

    if args.list:
        print_fields(args.input)

    if not args.output:
        logging.warning("warn: making modifications in-place")
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
