from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options


#This is to specify where we want the files to be downloaded
options = Options()
options.add_experimental_option("prefs", {
  "download.default_directory": r"/Users/charlesraymond/Desktop/Custodian_Folder",
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

#print(driver.page_source)

