import numpy as np
from statsmodels.distributions.empirical_distribution import ECDF
import colorednoise as cn




def generate_pink_noise(beta=1, freq=44100):
  """
  Function to generate pink noise (correlated noise)
  beta = 1 is for pink noise and is standard, 0 is Gaussian
  freq =  is just the number of samples 
  """
  noise_array = cn.powerlaw_psd_gaussian(beta, freq)      # Generate the noise
  noise_array = (noise_array - np.min(noise_array)) / (np.max(noise_array) - np.min(noise_array))     # Normalize
  noise_array = 2*noise_array - 1     # Scale between -1 and 1

  return noise_array

def to_uniform(beta, freq):
  """
  Function to transform noise into uniform distribution by using empirical CDF
  """

  arraydata = generate_pink_noise(beta, freq)

  cc = ECDF(arraydata.tolist())     # Calculates the empirical CDF of the scaled noise

  return cc(arraydata.tolist())
  