TITLE:
PDF Text Explorer Tool

AUTHOR:
Matthew Scott

OVERVIEW: 
This app takes a PDF file as input and produces some default summary statistics. 
The user can change the selection of input widgets to alter the extent of text pre-processing.
The user can input a word or term into the text input selection box to see the matching terms on 
individual concordance lines. The user can select any number of words present in the PDF file to see
a summary table with count of word frequency which can be downloaded as a csv.

FILES:
1. app.py
2. utils.py
3. pdf-docs folder (containing a selection of PDF files to test)
4. '.streamlit' folder (containing theme)

SET UP:
1. Download all files to a working directory.
2. If necessary, create a folder named 'pdf-docs' and move sample PDF's to this folder.
3. Open app.py and in the terminal type 'python -m streamlit run app.py' - this will create a
local server instance where the app can be displayed.
4. If the app doesn't open in the browser, click the link displayed in the terminal. 
5. Explore the app by uploading a PDF document from the correct folder and editing the user input widgets.
6. Optional: to load the theme, create a folder called '.streamlit' in the working directory and move the 
config.toml file into it. Streamlit will now use that theme on opening.