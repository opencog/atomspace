Using Python with the AtomSpace
===============================

Add these lines to the end of your ~/.bashrc file:
```
PYTHONPATH="$PYTHONPATH:/usr/local/share/opencog/python
export PYTHONPATH
```

Install the required dependencies:
```
cd ~/opencog/opencog/python
sudo pip install -r requirements.txt --upgrade
```

#### Client API

Once the instructions above have been performed, you can run Python scripts found in the ```~/opencog/opencog/python``` folder, and you can also download and use the Python Client API available here:
https://github.com/opencog/python-client
