# stdatadict 0.4.4

- **BugFix:** List of embedded tables in the form sheets was calculated wrongly 
  when centre tables are added as extra form type. This is now corrected.

# stdatadict 0.4.3

- **BugFix:** Adjust package styling functions to work with `openxlsx2` >= 1.22.

# stdatadict 0.4.2

- **Bugfix/Enhancent:** The project setup data files don't allow to distinguish
  between casenode forms and centre forms. The `read_metadata()` function has 
  a new (experimental) paramater: `detect_ctr_forms` that if set to `TRUE` 
  attempts to identify the centre forms by reading the headers of all data tables.
  This allows the data dictionary to display those forms as a seperate form 
  catagory. Otherwise it will be treated as a casenode form.

# stdatadict 0.4.1

- **New Colortheme** "num3" resembles the colors of of the num website 
  "https://www.netzwerk-universitaetsmedizin.de". (#9)
  
- **Enhancement** It is now possible to define the color of the "select all" 
  question independently from the colors of the "select" column. 
  

# stdatadict 0.4.0

- **New Function** `link_form_sheet()` adds links in the Form Overview table
  columns to the Form sheet in the excel file. Also adds a link to the Form 
  overview to each A1 cell in the form item sheets. (#3)

# stdatadict 0.3.0

- **Breaking Changes** 

  - `read_metadata()` replaces the parameters 
    `delim`, `decimal_mark`, and `encoding` by `...` paramters that will be passed
     to the `readr::read_delim()` function.
     
  - The internal function `read_meta_file()` is replaced by a copy of 
    `toTools::read_secutrial_table()`

  - **Rename** internal funcion `get_studyid()` -> `get_project_id()`
  
- **Enhancements**
  
  - `read_metadata()` has a `safe_read` option that attempts to repair csv input
     before passing it to `readr::read_delim()` this is because some exports
     contain csv files where some lines have a different number of 
     columns/seperators. (#8)
     
  - `read_metadata()` can now read `.xls` csv-files.

# stdatadict 0.2.1

- Add HTML Docs
- Publish Package on Github (Mirroring it from Gitlab)

- **Fix Bug** in `read_metadata()`: Variable types where parsed incorrectly when
  they have mostly missing values. This can be fixed by setting the `guess_max`
  parameter of `readr::read_delim()` to `Inf`.

# stdatadict 0.2.0

- **Fix Bugs**
  - create_fill() causes an error from the current openxlsx2 version causes an
    error when using the default parameter for patternType/pattern_type. 
    Therefore all patternType arguments have to set explicitly. (#5)
    
  - Creation of data dictionary tables now works if studys has multiple visitarms 
    (#4)
  - Creation of data dictionary tables now works if the hidden variable is not
    present in the visit plan (#4)

# stdatadict 0.1.1

- **Enhancement** `add_scope_selection()` can now omit the "select all" question.
  This is especially usefull if a section includes only one scope.
  
- **Fix Bug** When the Variable selection table has a Scope column with NA 
  entries the variable selection formular was created wrongly. This could be 
  fixed by replacing NA with an empty string.

# stdatadict 0.1.0

All basic functions for creating a variable selection Excel Workbook are now
included.

- **New Functions for Variable Scopes:** 

  - `create_scope_table()` helps to create a variable scope table
    from a simple data frame, that can then be used to join with the form item 
    tables.
  
  - `add_scope_selection()` adds variable scope selection tables (item packages)
    to the workbook. This can be used to select a predefined set of variables.
  
  - `add_varselect_settings()` Adds a hidden sheet to the workbook that holds 
    option-lists for drop down menu cells and some programming logic for the 
    variable scope selections.
    
- **New Color Theme Functions:**

  - `active_color_theme()` returns the name of the color theme currendtly used.
  - `get_color_theme()` prints how a color theme is defined. This might be 
     useful when a user wants to customize an existing color theme.  
  
- **Enhancements:**
  - Add heading 2 Style

# stdatadict 0.0.0.9203

- **New Functions** that help to add pre-styled elements, like paragraphs and 
  headings to the workbook.

- **New Function** `join_scopes()` adds a `Scope` column to the form item tables
  or merges with an existing one.

# stdatadict 0.0.0.9202

- **Fix Bug** the form order was changed after adding additional variable 
  information to the form item tables. It is now restored to it's original order.

# stdatadict 0.0.0.9201

- **Enhancement:** Add Capapability to add a variable selection column to the form item tables, 
when generating a data dictionary.

# stdatadict 0.0.0.9200

All needed functions for building a Data Dictionary Excel workbook are 
now included in the package.

- **Bug Fix** in `add_form_overview()`: When in sub_forms table every subform is 
  in a different mainform, then 'merge_cells' is an empty data frame. 
  The attempt to merge all rows in this empty data frame fails with an error. -> 
  now there is a check for if 'merge_cells' is empty.
  
- **Enhancement** in `add_form_overview()` displays a "not available"" message 
  instead of an empty table when certain type of forms is not available in the 
  data set.

# stdatadict 0.0.0.9106

- **New Function** `create_datadict()` creates a data dictionary excel workbook
  from data_dictionary tables (Form Overview + Form Items tables). It wraps the 
  functions `add_form_overview()` and `add_form_sheets()`
  
- **Bug Fixes:**
  - studyname of HAP without under score
  - subtitles fails if str_glue class -> convert to character
  - join_with_form_items fails if joining data frame has no column named "maincol"

# stdatadict 0.0.0.9105

- **New Function** `join_with_form_items` helps to join extra variable 
  informations to the form item tables.

# stdatadict 0.0.0.9104

- **New Color Themes** 
  - aquamarin
  - dzhk
  - napkon

- **New Functions**
  - `get_export_date()` reads the date of the export creation from the 
     ExportOptions.html file or approximates it from the file modification date 
     of the export.
     
  - `add_form_sheets()` adds the form sheet tables to the workbook.

# stdatadict 0.0.9103

- Add style rules to the workbook.

- **New Function** `add_form_overview()` adds the Form Overview tables to the 
  workbook.

# stdatadict 0.0.9102

- **New Functions** for creating, modifying and unsing color themes.

# stdatadict 0.0.9101

- **New Functions** for creating an excel workbook:
   - `wb_insert_row()` inserts a Worksheet into the Workbook.
   - `insert_worksheet()` emulates the function of inserting one or multiple 
      rows in a data sheet. 

# stdatadict 0.0.0.9100

Functions for calculating Data dictionary tables are completed.

- **New Function** `create_datadict_tables()` creates a list of form overview 
   tables and form item tables which form the base of the data dictionary.
   
- **Enhancement** `create_datadict_tables()` detects if the `hidden` variable
  is inverted in the export and reinverts it.

# stdatadict 0.0.0.9006

- **New Internal Function** to build the tables for the form item tables

# stdatadict 0.0.0.9005

- Add multi language support (de/en). Now the user can choose between German and
  English descriptions and table column names.


# stdatadict 0.0.0.9004

- **New Internal Function** to build the tables for the form overview.

# stdatadict 0.0.0.9003

- **New Internal Function** `get_studyid()` attempts to identify the study id 
  from the `formtablename` column, in order to remove it and the "mnp" prefix 
  from the variable entries.

# stdatadict 0.0.0.9002 

- New function to read study data files, which is also able to read data exported
  with the long names option.

# stdatadict 0.0.0.9001

- Add Readme
- Initiate Package structure

# stdatadict 0.0.0.9000

- Initial commit
