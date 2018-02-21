import urllib.request
import json
import datetime
import os
import pickle
import sys
import io
from api_function import api_update

sys.stdout = io.TextIOWrapper(sys.stdout.detach(), encoding = 'utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.detach(), encoding = 'utf-8')

api_update()
