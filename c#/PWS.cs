using System;
using System.Collections.Generic;
using System.Text;

namespace PWSLibrary
{
    public delegate RangResult PTFunc(double P, double T);
    public class GibbsRegion: IGibbsEnergy
    {
        private readonly Dictionary<Region, GibbsEnergy> energies = new Dictionary<Region, GibbsEnergy>();
        private Region region;
        private GibbsEnergy energy;
        private double pres;
        private double temp;

        public double Pres { get => pres; }
        public double Temp { get => temp; }
        public Region Region { get => region; }        

        public GibbsRegion() 
        {
            region = Region.Unknown;
        }
        public GibbsRegion(double P, double T): this ()
        {
            SetValues(P, T);    
        }

        protected bool ValidRegion ()
        {
            return region != Region.Unknown;
        }
        protected GibbsEnergy CreateEnergy(Region region)
        {
            switch (region)
            {
                case Region.Region1:
                    return new Region1();

                case Region.Region2:
                    return new Region2();

                case Region.Region5:
                    return new Region5();

                default:
                    return null;
            }            
        }
        protected Region GetRegion(double P, double T)
        {
            KeyValuePair<PTFunc, Region>[] region = {
                new KeyValuePair<PTFunc, Region>(Region1.GetInRange, Region.Region1),
                new KeyValuePair<PTFunc, Region>(Region2.GetInRange, Region.Region2),
                //new KeyValuePair<PTFunc, Region>(Region3.InRange, Region.Region3),
                new KeyValuePair<PTFunc, Region>(Region5.GetInRange, Region.Region5)
                };

            return Array.Find(region, x => x.Key(P, T) == RangResult.Ok).Value;
        }
        protected void SetEnergy(Region region)
        {            
            if (region == Region.Unknown)
            {
                energy = null;
                return;
            }

            if (energies.ContainsKey(region))
            {
                energies.TryGetValue(region, out energy);
                return;
            }

            energy = CreateEnergy(region);
            if (energy != null)
                energies.Add(region, energy);
        }
        
        public bool SetValues(double P, double T)
        {
            if ((Pres != P) || (Temp != T))
            {
                pres = P;
                temp = T;
                region = GetRegion(P, T);
                SetEnergy(region);
            }
            return region != Region.Unknown;
        }

        // Specific volume v(MPa, K) => (m3 kg–1)
        public double SpecificVolume()
        {
            return energy.SpecificVolume(Pres, Temp);
        }
        public double SpecificVolume(double P, double T)
        {
            SetValues(P, T);            
            return ValidRegion()? energy.SpecificVolume(P, T): 0;
        }

        // Specific internal energy u(MPa, K) => kJ kg–1
        public double SpecificInternalEnergy()
        {
            return energy.SpecificInternalEnergy(Pres, Temp);
        }
        public double SpecificInternalEnergy(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpecificInternalEnergy(P, T) : 0;
        }

        // Specific entropy s(MPa, K) => kJ kg–1 K–1
        public double SpecificEntropy()
        {
            return energy.SpecificEntropy(Pres, Temp);
        }
        public double SpecificEntropy(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpecificEntropy(P, T) : 0;
        }

        // Specific enthalpy h(MPa, K) => kJ kg–1
        public double SpecificEnthalpy()
        {
            return energy.SpecificEnthalpy(Pres, Temp);
        }
        public double SpecificEnthalpy(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpecificEnthalpy(P, T) : 0;
        }
        // Specific isobaric heat capacity cp(MPa, K) => kJ kg–1 K–1
        public double SpecificIsobaricHeatCapacity()
        {
            return energy.SpecificIsobaricHeatCapacity(Pres, Temp);
        }
        public double SpecificIsobaricHeatCapacity(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpecificIsobaricHeatCapacity(P, T) : 0;
        }

        // Specific isochoric heat capacity ct 
        public double SpecificIsochoricHeatCapacity()
        {
            return energy.SpecificIsochoricHeatCapacity(Pres, Temp);
        }
        public double SpecificIsochoricHeatCapacity(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpecificIsochoricHeatCapacity(P, T) : 0;
        }

        // Speed of sound  w(MPa, K) => m s–1
        public double SpeedOfSound()
        {
            return energy.SpeedOfSound(Pres, Temp);
        }
        public double SpeedOfSound(double P, double T)
        {
            SetValues(P, T);
            return ValidRegion() ? energy.SpeedOfSound(P, T) : 0;
        }
    }
    public static class PWS
    {
        public static readonly string Version = "IAPWS R7-97(2012)";

        public static readonly string DensityUnits = "kg mˉ³";
        public static readonly string HeatCapacityUnits = "kJ kgˉ¹ Kˉ¹";
        public static readonly string PressureUnits = "MPa";
        public static readonly string SpecificEntropyUnits = "kJ kgˉ¹ Kˉ¹";
        public static readonly string SpecificEnthalpyUnits = "kJ kgˉ¹";
        public static readonly string SpecificInternalEnergyUnits = "kJ kgˉ¹";
        public static readonly string SpecificVolumeUnits = "m3 kgˉ¹";        
        public static readonly string SpeedOfSoundUnits = "m sˉ¹";
        public static readonly string TemperatureUnits = "K";

        // The specific gas constant of ordinary water
        public static readonly double R = 0.461526;         // kJ kgˉ¹ Kˉ¹

        // The values of the critical parameters
        public static readonly double Tc = 647.096;         // K
        public static readonly double Pc = 22.064;          // MPa
        public static readonly double Dc = 322;             // kg m-3

        // Since the 5th International Conference on the Properties of Steam in London in 1956, the
        // specific internal energy and the specific entropy of the saturated liquid at the triple point have
        // been set equal to zero
        public static readonly double Tt = 273.16;          // K
        public static readonly double Pt = 611.657E-6;      // MPa
        public static readonly double Ut = 0;               // kJ kgˉ¹
        public static readonly double St = 0;               // kJ kgˉ¹ Kˉ¹

        // Specific enthalpy of the saturated liquid at the triple point
        public static readonly double Ht = 0.611783E-3;     // kJ kgˉ¹

        // Temperature and pressure range const
        public static readonly double TEMP_BORDER_0 = 273.15;    // K
        public static readonly double TEMP_BORDER_1 = 623.15;    // K
        public static readonly double TEMP_BORDER_2 = 863.15;    // K
        public static readonly double TEMP_BORDER_3 = 1073.15;   // K
        public static readonly double TEMP_BORDER_4 = 2273.15;   // K

        public static readonly double PRES_BORDER_0 = 0;            // MPa
        public static readonly double PRES_BORDER_1 = 50;           // MPa
        public static readonly double PRES_BORDER_2 = 100;          // MPa

        public static double LeftPressureBorder(double T) {
            return PRES_BORDER_0;
        }
        public static double LeftTemperatureBorder(double P)
        {
            return TEMP_BORDER_0;
        }
        public static double RightPressureBorder(double T)
        {
            return (T <= TEMP_BORDER_3) ? PRES_BORDER_2 : PRES_BORDER_1;
        }
        public static double RightTemperatureBorder(double P)
        {
            return (P <= PRES_BORDER_1) ? TEMP_BORDER_4 : TEMP_BORDER_3;
        }

        public static RangResult InRange (double P, double T)
        {
            if (P < PRES_BORDER_0) return RangResult.InvalidP;
            if (T < TEMP_BORDER_0) return RangResult.InvalidT;

            if ((T < TEMP_BORDER_3) && (P > PRES_BORDER_2)) return RangResult.InvalidP;
            if ((T < TEMP_BORDER_4) && (P > PRES_BORDER_1)) return RangResult.InvalidP;

            return RangResult.Ok;
        }

        #region Helmholtz Internal Energy
        public static IHelmholtzEnergy GetHelmholtzEnergy(double P, double T)
        {
            //if (Region3.InRange(P, T))
                return new Region3();

            //return null;
        }
        public static double? GetHelmholtzPressure(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);                        
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.Pressure(D, T) : (double?) null;
       }
        public static double? GetHelmholtzSpecificInternalEnergy(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpecificInternalEnergy(D, T) : (double?) null;
        }
        public static double? GetHelmholtzSpecificEntropy(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpecificEntropy(D, T)  : (double?)null;            
        }
        public static double? GetHelmholtzSpecificEnthalpy(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpecificEnthalpy(D, T) : (double?)null;
        }
        public static double? GetHelmholtzSpecificIsobaricHeatCapacity(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpecificIsobaricHeatCapacity(D, T) : (double?)null;
        }
        public static double? GetHelmholtzSpecificIsochoricHeatCapacity(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpecificIsochoricHeatCapacity(D, T) : (double?)null;
        }
        public static double? GetHelmholtzSpeedOfSound(double D, double T)
        {
            var h = GetHelmholtzEnergy(0, T);
            return Region3.InRange(h.Pressure(D, T), T) == RangResult.Ok ? h.SpeedOfSound(D, T) : (double?)null;
        }
        #endregion
    }
}
