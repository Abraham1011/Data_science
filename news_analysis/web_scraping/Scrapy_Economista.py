#!/usr/bin/env python
# coding: utf-8

from selenium import webdriver
import pandas as pd
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
import random
from time import sleep
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from datetime import datetime
import chromedriver_binary 


x1 = list() # Titulo
x2 = list() # Texto
x3 = list() # Autor
x4 = list() # Fecha
x5 = list() # URL



#driver =  webdriver.Edge()
driver =  webdriver.Firefox()

driver.get('https://www.eleconomista.com.mx/seccion/economia')

for i in range(10):
	try:
		element = driver.find_element(By.XPATH, '//button[text()="Ver más noticias"]')
		driver.execute_script("arguments[0].click();", element)
		sleep(2)
	except:
		break

links_notas = driver.find_elements(By.XPATH,'//h3[@class="jsx-578919967 title"]/a[@href]')
#jsx-578919967 title
#jsx-3595501718
link = []

for i in links_notas:
	link.append(i.get_attribute("href"))



for k in link:
	try:
		x5.append(k)
		driver.get(k)
		#sleep(.5)
	except Exception as e:
		print("Error al entra al link")

	try:
		x1.append(driver.find_element(By.XPATH,'//h1[@class="jsx-891979598"]').text) #Titulo
		print(driver.find_element(By.XPATH,'//h1[@class="jsx-891979598"]').text)
	except Exception as e:
		print("Error al guardar titulo")
		x1.append("")


	try:
		textos = driver.find_elements(By.XPATH, '//div[@class="jsx-537108098"]/p')

		txt = textos[0].text
		for texto in range(1,len(textos)):
			txt = txt + " " + textos[texto].text

		x2.append(txt) #Texto

	except Exception as e:
		print("Error al guradar texto")
		x2.append("")

	try:
		x3.append(driver.find_element(By.XPATH,'//div[@class="jsx-2090697612"]/p/a[@title]').text) #Autor
	except Exception as e:
		try:
			x3.append(driver.find_element(By.XPATH,'//div[@class="jsx-2090697612"]/p/span[@class="jsx-3444981233 "]').text) #Autor
		except:
			x3.append("")
			print("Error al guardar el Autor")

	try:
		x4.append(driver.find_element(By.XPATH,'//time[@class="jsx-1677808495"]').text) #Fecha
	except Exception as e:
		print("Error al guardar fecha")
		x4.append("")




df = pd.DataFrame()
df["Tiutlo"] = x1
df["Texto"] = x2
df["Autor"] = x3
df["Fecha"] = x4
df["URL"] = x5

print(df)
print(df.shape)
df.to_csv("df.csv")

