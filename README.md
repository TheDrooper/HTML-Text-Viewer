# HTML-Text-Viewer
This is a Progress OpenEdge program that will display the HTML text from a web page. The program has the following fields:
URL: The URL of the web page, including any arguments.
IP: The web site's IP address. This is needed since there is no DNS lookup being done.
Port: This is generally 80 but can be changed if necessary.
Timeout: The number of seconds to wait for a response.

When you click the Send Request button, a socket is created to the IP/port and the web page is downloaded. The HTML text from the page is displayed in the Reply editor box. The Status and Time fields will show you what process is currently being performed and when the status was last updated.

This program will remember the last values entered into the fields via a configuration file, htmltextview.cfg.

Click the Exit button to exit the program.
