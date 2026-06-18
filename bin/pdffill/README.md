This is a command line utility to manipulate a PDF's fields. 

$ pip install pdffill

# List field names
$ pdffill input.pdf -l

# Rename some fields
$ pdffill.py input.pdf -r text_3uwum=seller text_4vpsr=buyer text_5ovhg=address

# Fill in some fields, write to a separate output file
$ pdffill.py input.pdf -o filled.pdf -f "seller=Wollam Bonnie C Estate" "buyer=John Doe" address=20235 Majestic Street, Orlando, Fl, 32833

# Delete some fields, write to a separate output file
$ pdffill.py input.pdf -o slim.pdf -d balance-to-close -d purchase-price-other-item -d purchase-price-other-item-value

# Fill in all fields with their own names, helpful for seeing where fields are on the page.
$ pdffill.py input.pdf -o debug.pdf -F

This utility doesn't handle checkboxes well, and it is recommended to use plain text fields instead.