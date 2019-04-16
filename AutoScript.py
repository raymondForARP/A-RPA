from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
from selenium import webdriver
from selenium.webdriver.support.ui import Select
import pandas as pd
from time import sleep
import zipfile
import os
import xlrd
import csv
from shutil import copyfile
from datetime import date


#This is to specify where we want the files to be downloaded
options = Options()
options.add_experimental_option("prefs", {
  "download.default_directory": r"/Users/charlesraymond/A-RPA",
  "download.prompt_for_download": False,
  "download.directory_upgrade": True,
  "safebrowsing.enabled": True
})

#Creating driver and opening up Http
driver = webdriver.Chrome(executable_path="/Users/charlesraymond/Documents/chromedriver",options=options)
driver.get("https://www.oneclickreporting.com")

#Load in login info and submit
driver.find_element_by_id("id_en").send_keys("TFrenzel")
driver.find_element_by_id ("password_en").send_keys("%dfe45NB")
driver.find_element_by_id("anmeldenButton_en").click()

#Click link
driver.find_element_by_link_text("E-Reporting").click()

#Opens new browser, 
driver.switch_to.window(driver.window_handles[-1]) 

#clicking links
driver.find_element_by_link_text("OK").click()
driver.find_element_by_link_text("Manage").click()

#clicking links
driver.find_element_by_css_selector("a[href= \"javascript:load('117745')\"]").click()
driver.find_element_by_link_text("OK").click()
driver.find_element_by_link_text("Select all").click()
driver.find_element_by_link_text("as xls").click()
driver.find_element_by_link_text("No").click()

#Sits and waits for zip file.
#Once downloaded, zip will appear
zipFiles = []

while not zipFiles:
    for file in os.listdir("/Users/charlesraymond/A-RPA"):           
        if file.endswith(".zip"):
            zipFiles.append(file)
            print(file)

    sleep(3)

    
zip_ref = zipfile.ZipFile(zipFiles[0], 'r')
    
    
zip_ref.extractall()
zip_ref.close() 
#print(zipFiles[x])

#os.remove(zipFiles[0])

#Gets todays date to document zip file
today = str(date.today())
print(today)

date = today + ".zip"
date = "/Users/charlesraymond/A-RPA/zipFiles/" + date
copyfile(zipFiles[0], date)
print("This is the zip file name" + date)
os.remove(zipFiles[0])

zipFiles.pop()
xlsFiles = []

for file in os.listdir("/Users/charlesraymond/A-RPA"):
    if file.endswith(".xls"):
            #print(type(file))
        print(file)
        print("WE GET HERE")
            #thefile = file.to_string()
        data_xls = pd.read_excel(file, encoding_override="cp1251" )
            #data_xls = xlrd.open_workbook(file, logfile=open(os.devnull, 'w'))#encoding_override='cp1252') 
            
            #your_csv_file = open('someFile.csv','data_xls')
            #wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)
        file = file.replace(".xls", ".csv")
        print("new file name" + file)
           # for fownum in xrange(sh.nrows):
               # wr.writerow(sh.row_values(rownum))
           # your_csv_file.close()
            #data_xls.to_csv(file, encoding='utf-8', index=False)
        data_xls.to_csv(file, encoding='utf-8', index=False)

    #print(zipFiles[0])
#sleep(3)

#print(driver.page_source)

