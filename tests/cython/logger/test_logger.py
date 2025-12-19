#! /usr/bin/env pytest

from unittest import TestCase
import os
import tempfile

import opencog.logger

class LoggerTest(TestCase):

    def setUp(self):
        tempfd, self.tempfn = tempfile.mkstemp()
        # close the temp file as Logger will want to manually
        # open it
        os.close(tempfd)
        self.log = opencog.logger.create_logger(self.tempfn)
        # Enable synchronous logging so messages are written immediately
        self.log.set_sync(True)

    def tearDown(self):
        del self.log
        os.remove(self.tempfn)

    def test_log_levels(self):
        self.log.set_level("ERROR")
        self.assertFiltersCorrectly(["ERROR"],["WARN", "INFO", "DEBUG", "FINE"])
        self.log.set_level("WARN")
        self.assertFiltersCorrectly(["ERROR","WARN"],["INFO", "DEBUG", "FINE"])
        self.log.set_level("INFO")
        self.assertFiltersCorrectly(["ERROR","WARN","INFO"],["DEBUG", "FINE"])
        self.log.set_level("DEBUG")
        self.assertFiltersCorrectly(["ERROR","WARN","INFO","DEBUG"],["FINE"])
        self.log.set_level("FINE")
        self.assertFiltersCorrectly(["ERROR","WARN","INFO","DEBUG","FINE"],[])

    def assertFiltersCorrectly(self,lvls_displayed,lvls_muted):
        file_size = os.path.getsize(self.tempfn)
        print(f"\n=== Current log level: {self.log.get_level()} ===")
        print(f"Initial file size: {file_size}")
        for lvlname in lvls_muted:
            lvl = self.log.string_as_level(lvlname)
            print(f"Muted: {lvlname} (value={lvl})")
            self.log.log(lvl,"these messages should be muted")
            new_size = os.path.getsize(self.tempfn)
            print(f"  File size after: {new_size} (expected same)")
            self.assertEqual(file_size, new_size)

        for lvlname in lvls_displayed:
            lvl = self.log.string_as_level(lvlname)
            print(f"Displayed: {lvlname} (value={lvl})")
            self.log.log(lvl,"this should appear")
            new_size = os.path.getsize(self.tempfn)
            print(f"  File size: {file_size} -> {new_size} (expected growth)")
            # Show file content on failure
            if not (file_size < new_size):
                with open(self.tempfn, 'r') as f:
                    print(f"  File content: {repr(f.read())}", file=sys.stderr)
            self.assertTrue(file_size < new_size)
            file_size = new_size

    def test_get_set_log_level(self):
        for lvlname in self.log.level_order():
            self.log.set_level(lvlname)
            self.assertEqual(self.log.get_level(),lvlname)


