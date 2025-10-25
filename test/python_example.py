#!/usr/bin/env python3
"""
Data processing utilities
"""

import numpy as np
import pandas as pd

<<<<<<< HEAD
def load_data(filename, use_cache=True):
    """Load data from file with optional caching"""
    if use_cache:
        # Try to load from cache
        cache_file = filename + '.cache'
        if os.path.exists(cache_file):
            return pd.read_pickle(cache_file)

    # Load from original file
    data = pd.read_csv(filename)

    if use_cache:
        data.to_pickle(cache_file)

    return data
=======
def load_data(filename):
    """Load data from CSV file"""
    return pd.read_csv(filename, parse_dates=True, index_col=0)
>>>>>>> feature-simple-loader

class DataProcessor:
    def __init__(self, data):
        self.data = data
        self.processed = False

<<<<<<< HEAD
    def normalize(self, method='minmax'):
        """Normalize data using specified method"""
        if method == 'minmax':
            # Min-max normalization
            self.data = (self.data - self.data.min()) / (self.data.max() - self.data.min())
        elif method == 'zscore':
            # Z-score normalization
            self.data = (self.data - self.data.mean()) / self.data.std()
        else:
            raise ValueError(f"Unknown normalization method: {method}")

        self.processed = True
=======
    def normalize(self):
        """Normalize data using z-score"""
        self.data = (self.data - self.data.mean()) / self.data.std()
        self.processed = True
>>>>>>> feature-zscore-only

    def save(self, filename):
        """Save processed data"""
        self.data.to_csv(filename, index=False)

if __name__ == "__main__":
    data = load_data("input.csv")
    processor = DataProcessor(data)
    processor.normalize()
    processor.save("output.csv")
