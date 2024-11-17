import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Patient } from '../domain/patient';
import { catchError, Observable, of } from 'rxjs';
import { EditPatient } from '../domain/edit-patient';

@Injectable({
  providedIn: 'root'
})
export class PatientService {

  theServerURL = 'https://localhost:5001/api/Patient';
  token = `eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1laWRlbnRpZmllciI6IjAyOWUxYWEzLTRjZTUtNDA0ZS1iMTBkLTZiZTUwZDhhMmZjYSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL2VtYWlsYWRkcmVzcyI6InJpdGFiYXJib3NhMjYucmJAZ21haWwuY29tIiwiaHR0cDovL3NjaGVtYXMubWljcm9zb2Z0LmNvbS93cy8yMDA4LzA2L2lkZW50aXR5L2NsYWltcy9yb2xlIjoiUGF0aWVudCIsImV4cCI6MTczMTc4NzEzNiwiaXNzIjoiSGVhbHRoY2FyZVN5c3RlbSIsImF1ZCI6IkhlYWx0aGNhcmVTeXN0ZW1Vc2VyIn0.HfKFbJnkSpY-pAx9SOxXpYPg_xVeRi_fPLD-Jrc-TfA`;
  httpOptions = {
    headers: new HttpHeaders({ 
      'Content-Type': 'application/json'
       })
  };

  constructor(private messageService: MessageService, private http: HttpClient) { }
  public updateProfile(name: string, phone: string, email: string, address: string, emergencyContact: string) {
    const url = `${this.theServerURL}`;
  
    // Construct the patient object, including only fields that have values
    let patient: EditPatient = {};
  
    if (name && name.trim() !== "") {
      patient.name = name;
    }
    if (phone && phone.trim() !== "") {
      patient.phone = phone;
    }
    if (email && email.trim() !== "") {
      patient.email = email;
    }
    if (address && address.trim() !== "") {
      patient.address = address;
    }
    if (emergencyContact && emergencyContact.trim() !== "") {
      patient.emergencyContact = emergencyContact;
    }
    this.log(`${url}`)
    // Make the PUT request
    this.http.put<Patient>(url, patient, this.httpOptions)
      .pipe(catchError(this.handleError<Patient>('Update patient profile')))
      .subscribe(data => {
        this.log(`Patient profile: was successfully updated.`);
      });
  }
  

  public createPatientProfile(firstName: string, lastName: string, phone: string, email: string, address: string, emergencyContact: string, gender: string, dateBirth: string) {
    const url = `${this.theServerURL}/Create-PatientProfile`;
    let patient: Patient = {
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      datebirth: dateBirth,
      emergencyContact: emergencyContact,
      gender: gender,
    };
    
    this.http.post<Patient>(url, patient, this.httpOptions)
      .pipe(catchError(this.handleError<Patient>('Create patient profile')))
      .subscribe(data => {
        this.log(`Patient profile: ${data.email} was successfully created.`);
      });
  }

 

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if (error.status === 200 || error.status === 400) {
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured. [${error.message}]`);
      }
      return of(result as T);
    };
  }

  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
