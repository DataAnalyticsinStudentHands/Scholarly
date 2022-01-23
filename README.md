# Scholarly

Scholarly is a collection of scripts to automatically scrape Google Scholar publication records for individuals.

- **Inputs**: List of Google Scholar profile IDs + a designated MongoDB database.
- **Outputs**: Two MongoDB collections: 
  - Google Profiles : Contains the "header" information from each Google Profile
  - Publications : Contains a document with detailed publication for each publication listed for the input profiles.

It can be used for bibliometric analysis, calculation of metrics, and more.

Scholarly is designed for unsupervised use and to be distributed across multiple machines (with different IP addresses) to speed the collection process for large tranches of scholar profiles.

(https://github.com/peggylind/ScholarballExtended/wiki)[See full wiki for instructions & technical details]
